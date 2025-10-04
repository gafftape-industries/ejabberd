%%%-------------------------------------------------------------------
%%% File    : mod_session_auth.erl
%%% Author  : Custom
%%% Purpose : Session-based authentication with external API
%%% Created : 2025
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(mod_session_auth).

-author('custom').

-behaviour(gen_mod).
-behaviour(ejabberd_auth).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3, mod_doc/0,
         depends/2, mod_opt_type/1, mod_options/1]).

%% ejabberd_auth callbacks
-export([check_password/4, user_exists/2, store_type/1, plain_password_required/1]).

%% Hook handlers
-export([on_user_available/2, on_user_unavailable/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

-define(SESSION_TABLE, mod_session_auth_sessions).

%%%===================================================================
%%% gen_mod callbacks
%%%===================================================================

start(Host, _Opts) ->
    %% Initialize HTTP client
    application:ensure_all_started(hackney),

    %% Create ETS table to store sessionID mappings
    ets:new(?SESSION_TABLE, [named_table, public, set, {read_concurrency, true}]),

    %% Register as auth backend
    ejabberd_auth:register_backend(Host, ?MODULE),

    %% Register hooks
    {ok, [
        {hook, user_available, on_user_available, 50},
        {hook, user_unavailable, on_user_unavailable, 50}
    ]}.

stop(Host) ->
    %% Unregister auth backend
    ejabberd_auth:unregister_backend(Host, ?MODULE),

    %% Clean up ETS table
    catch ets:delete(?SESSION_TABLE),
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

%%%===================================================================
%%% ejabberd_auth callbacks
%%%===================================================================

-spec check_password(binary(), binary(), binary(), binary()) -> boolean().
check_password(User, _AuthzId, Server, Password) ->
    %% Password is the sessionID
    SessionID = Password,
    ApiUrl = mod_session_auth_opt:api_url(Server),
    Timeout = mod_session_auth_opt:timeout(Server),

    ValidateUrl = <<ApiUrl/binary, "/validate">>,

    RequestBody = jiffy:encode(#{
        <<"sessionID">> => SessionID,
        <<"user">> => User
    }),

    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Options = [{recv_timeout, Timeout}, {connect_timeout, Timeout}],

    case hackney:post(ValidateUrl, Headers, RequestBody, Options) of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    try
                        #{<<"room">> := RoomName} = jiffy:decode(Body, [return_maps]),
                        %% Sanitize room name to prevent injection attacks
                        SanitizedRoom = sanitize_room_name(RoomName),
                        %% Store sessionID and room name for later use in hooks
                        UserKey = {User, Server},
                        ets:insert(?SESSION_TABLE, {UserKey, SessionID, SanitizedRoom}),
                        ?INFO_MSG("Session auth success for ~ts@~ts, room: ~ts",
                                 [User, Server, SanitizedRoom]),
                        true
                    catch
                        _:_ ->
                            ?ERROR_MSG("Invalid API response format for ~ts@~ts",
                                      [User, Server]),
                            false
                    end;
                {error, Reason} ->
                    ?ERROR_MSG("Failed to read API response for ~ts@~ts: ~p",
                              [User, Server, Reason]),
                    false
            end;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            ?WARNING_MSG("Session auth failed for ~ts@~ts: HTTP ~p",
                        [User, Server, StatusCode]),
            false;
        {error, Reason} ->
            ?ERROR_MSG("Session auth API error for ~ts@~ts: ~p",
                      [User, Server, Reason]),
            false
    end.

-spec user_exists(binary(), binary()) -> boolean().
user_exists(_User, _Server) ->
    %% Always return true since we validate via API
    true.

-spec store_type(binary()) -> external.
store_type(_Server) ->
    external.

-spec plain_password_required(binary()) -> true.
plain_password_required(_Server) ->
    true.

%%%===================================================================
%%% Hook handlers
%%%===================================================================

on_user_available(Acc, {User, Server, Resource}) ->
    %% Retrieve sessionID and room name from ETS
    UserKey = {User, Server},
    case ets:lookup(?SESSION_TABLE, UserKey) of
        [{_, SessionID, RoomName}] ->
            case register_session(User, Server, SessionID) of
                ok ->
                    %% Try to join MUC, terminate session if it fails
                    case auto_join_muc(User, Server, Resource, RoomName) of
                        ok ->
                            ok;
                        error ->
                            ?ERROR_MSG("Failed to join MUC for ~ts@~ts, terminating session",
                                      [User, Server]),
                            terminate_session(User, Server, Resource)
                    end;
                error ->
                    ?ERROR_MSG("Failed to register session for ~ts@~ts, terminating session",
                              [User, Server]),
                    terminate_session(User, Server, Resource)
            end;
        [] ->
            %% Missing session data - this should never happen, terminate session
            ?ERROR_MSG("No session data found for ~ts@~ts, terminating session",
                      [User, Server]),
            terminate_session(User, Server, Resource)
    end,
    Acc.

on_user_unavailable(Acc, {User, Server, _Resource}) ->
    %% Retrieve sessionID from ETS
    UserKey = {User, Server},
    case ets:lookup(?SESSION_TABLE, UserKey) of
        [{_, SessionID, _RoomName}] ->
            unregister_session(User, Server, SessionID),
            %% Clean up ETS entry
            ets:delete(?SESSION_TABLE, UserKey);
        [] ->
            ?WARNING_MSG("No session data found for ~ts@~ts", [User, Server])
    end,
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Sanitize room name to prevent injection attacks
%% Only allow alphanumeric characters, hyphens, and underscores
-spec sanitize_room_name(binary()) -> binary().
sanitize_room_name(RoomName) ->
    %% Convert to lowercase and filter allowed characters
    Lowercase = string:lowercase(RoomName),
    Allowed = re:replace(Lowercase, <<"[^a-z0-9_-]">>, <<"">>, [global, {return, binary}]),
    %% Limit length to 64 characters
    case byte_size(Allowed) of
        Size when Size > 64 ->
            binary:part(Allowed, 0, 64);
        _ ->
            Allowed
    end.

-spec register_session(binary(), binary(), binary()) -> ok | error.
register_session(User, Server, SessionID) ->
    ApiUrl = mod_session_auth_opt:api_url(Server),
    Timeout = mod_session_auth_opt:timeout(Server),

    RegisterUrl = <<ApiUrl/binary, "/register">>,
    JID = jid:encode({User, Server, <<>>}),

    RequestBody = jiffy:encode(#{
        <<"sessionID">> => SessionID,
        <<"jid">> => JID
    }),

    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Options = [{recv_timeout, Timeout}, {connect_timeout, Timeout}],

    case hackney:post(RegisterUrl, Headers, RequestBody, Options) of
        {ok, StatusCode, _RespHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            hackney:skip_body(ClientRef),
            ?INFO_MSG("Registered session for ~ts@~ts", [User, Server]),
            ok;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            ?WARNING_MSG("Failed to register session for ~ts@~ts: HTTP ~p",
                        [User, Server, StatusCode]),
            error;
        {error, Reason} ->
            ?ERROR_MSG("Session register API error for ~ts@~ts: ~p",
                      [User, Server, Reason]),
            error
    end.

-spec unregister_session(binary(), binary(), binary()) -> ok | error.
unregister_session(User, Server, SessionID) ->
    ApiUrl = mod_session_auth_opt:api_url(Server),
    Timeout = mod_session_auth_opt:timeout(Server),

    UnregisterUrl = <<ApiUrl/binary, "/unregister">>,
    JID = jid:encode({User, Server, <<>>}),

    RequestBody = jiffy:encode(#{
        <<"sessionID">> => SessionID,
        <<"jid">> => JID
    }),

    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Options = [{recv_timeout, Timeout}, {connect_timeout, Timeout}],

    case hackney:post(UnregisterUrl, Headers, RequestBody, Options) of
        {ok, StatusCode, _RespHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            hackney:skip_body(ClientRef),
            ?INFO_MSG("Unregistered session for ~ts@~ts", [User, Server]),
            ok;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            ?WARNING_MSG("Failed to unregister session for ~ts@~ts: HTTP ~p",
                        [User, Server, StatusCode]),
            error;
        {error, Reason} ->
            ?ERROR_MSG("Session unregister API error for ~ts@~ts: ~p",
                      [User, Server, Reason]),
            error
    end.

%% Auto-join user to MUC room
-spec auto_join_muc(binary(), binary(), binary(), binary()) -> ok | error.
auto_join_muc(User, Server, Resource, RoomName) ->
    MucDomain = mod_session_auth_opt:muc_domain(Server),

    %% Validate room name is not empty after sanitization
    case RoomName of
        <<>> ->
            ?ERROR_MSG("Empty room name after sanitization for ~ts@~ts", [User, Server]),
            error;
        _ ->
            %% Construct full MUC JID: room@muc_domain/nickname
            RoomJID = <<RoomName/binary, "@", MucDomain/binary, "/", User/binary>>,

            %% Create presence stanza to join MUC room
            From = jid:make(User, Server, Resource),
            To = jid:decode(RoomJID),

            Presence = #presence{from = From, to = To},
            ejabberd_router:route(Presence),

            ?INFO_MSG("Auto-joining ~ts@~ts to ~ts", [User, Server, RoomJID]),
            ok
    end.

%% Terminate user session by closing their connection
-spec terminate_session(binary(), binary(), binary()) -> ok.
terminate_session(User, Server, Resource) ->
    JID = jid:make(User, Server, Resource),
    ejabberd_sm:close_session(JID, <<"MUC join failed">>),
    ?INFO_MSG("Terminated session for ~ts@~ts/~ts", [User, Server, Resource]),
    ok.

%%%===================================================================
%%% Configuration
%%%===================================================================

depends(_Host, _Opts) ->
    [].

mod_opt_type(api_url) ->
    econf:binary();
mod_opt_type(timeout) ->
    econf:timeout(millisecond);
mod_opt_type(muc_domain) ->
    econf:binary().

mod_options(Host) ->
    ConferenceDomain = <<"conference.", Host/binary>>,
    [{api_url, <<"http://localhost:3000">>},
     {timeout, 5000},
     {muc_domain, ConferenceDomain}].

mod_doc() ->
    #{desc =>
          ?T("This module provides session-based authentication via external API. "
             "It validates sessionIDs (passed as passwords) against an HTTP endpoint "
             "and manages user session registration/unregistration. The API returns "
             "a room name which is sanitized and used to auto-join users to a MUC room. "
             "If MUC join fails, the user session is terminated."),
      opts =>
          [{api_url,
            #{value => ?T("URL"),
              desc =>
                  ?T("Base URL of the external authentication API. "
                     "The module will append /validate, /register, and /unregister endpoints. "
                     "The /validate endpoint must return JSON with 'room' field. "
                     "Default: http://localhost:3000")}},
           {timeout,
            #{value => ?T("timeout()"),
              desc =>
                  ?T("Timeout in milliseconds for API requests. "
                     "The default value is '5000' (5 seconds).")}},
           {muc_domain,
            #{value => ?T("Domain"),
              desc =>
                  ?T("Domain of the MUC service where rooms are located. "
                     "Room names from API are combined with this domain to form full JIDs. "
                     "Default: conference.domain")}}],
      note =>
          ?T("Room names are sanitized to only allow alphanumeric characters, "
             "hyphens, and underscores (max 64 chars). Invalid room names will "
             "cause session termination.")}.
