%%%-------------------------------------------------------------------
%%% File    : mod_group_filter.erl
%%% Author  : Custom
%%% Purpose : Filter messages based on dynamic group membership via API
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
-module(mod_group_filter).

-author('custom').

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, reload/3, mod_doc/0,
         depends/2, mod_opt_type/1, mod_options/1]).

-export([filter_packet/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

%%%===================================================================
%%% Callbacks and hooks
%%%===================================================================
start(Host, _Opts) ->
    %% Initialize HTTP client (hackney is included in ejabberd)
    application:ensure_all_started(hackney),
    %% Create ETS table for caching authorization results
    CacheTable = cache_table_name(Host),
    ets:new(CacheTable, [named_table, public, set, {read_concurrency, true}]),
    {ok, [{hook, filter_packet, filter_packet, 50}]}.

stop(Host) ->
    %% Clean up ETS table
    CacheTable = cache_table_name(Host),
    catch ets:delete(CacheTable),
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec filter_packet({jid(), jid(), stanza()}) ->
    {jid(), jid(), stanza()} | drop.
filter_packet({From, To, #message{} = Packet} = Input) ->
    LServer = To#jid.lserver,
    case is_c2c_message(From, To) of
        true ->
            case check_group_authorization(From, To, LServer) of
                {ok, true} ->
                    Input;
                {ok, false} ->
                    %% Permission denied - not in same group
                    Err = xmpp:make_error(Packet,
                        xmpp:err_not_authorized(
                            <<"Not authorized to message this user">>,
                            xmpp:get_lang(Packet))),
                    ejabberd_router:route(To, From, Err),
                    drop;
                {error, Reason} ->
                    %% API failure - fail secure by blocking
                    ?ERROR_MSG("Group filter API error for ~ts -> ~ts: ~p",
                               [jid:encode(From), jid:encode(To), Reason]),
                    Err = xmpp:make_error(Packet,
                        xmpp:err_service_unavailable(
                            <<"Service temporarily unavailable">>,
                            xmpp:get_lang(Packet))),
                    ejabberd_router:route(To, From, Err),
                    drop
            end;
        false ->
            %% Not a user-to-user message, allow it
            Input
    end;
filter_packet(Acc) ->
    %% Not a message packet
    Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Get cache table name for this host
-spec cache_table_name(binary()) -> atom().
cache_table_name(Host) ->
    binary_to_atom(<<"mod_group_filter_cache_", Host/binary>>, utf8).

%% Check if this is a client-to-client message (not system, not groupchat, etc)
-spec is_c2c_message(jid(), jid()) -> boolean().
is_c2c_message(From, To) ->
    %% Only filter messages between users (not empty user parts)
    From#jid.luser =/= <<"">> andalso To#jid.luser =/= <<"">>.

%% Call external API to check if From is authorized to message To
-spec check_group_authorization(jid(), jid(), binary()) ->
    {ok, boolean()} | {error, term()}.
check_group_authorization(From, To, LServer) ->
    CacheTTL = mod_group_filter_opt:cache_ttl(LServer),
    FromJid = jid:encode(jid:remove_resource(From)),
    ToJid = jid:encode(jid:remove_resource(To)),

    %% Check cache if enabled (TTL > 0)
    case CacheTTL of
        0 ->
            %% Cache disabled, make API call directly
            call_api(FromJid, ToJid, LServer);
        _ ->
            %% Cache enabled, check cache first
            CacheKey = {FromJid, ToJid},
            CacheTable = cache_table_name(LServer),
            case lookup_cache(CacheTable, CacheKey, CacheTTL) of
                {ok, Allowed} ->
                    {ok, Allowed};
                miss ->
                    %% Cache miss, make API call and cache result
                    case call_api(FromJid, ToJid, LServer) of
                        {ok, Allowed} = Result ->
                            store_cache(CacheTable, CacheKey, Allowed),
                            Result;
                        Error ->
                            Error
                    end
            end
    end.

%% Look up authorization result in cache
-spec lookup_cache(atom(), {binary(), binary()}, non_neg_integer()) ->
    {ok, boolean()} | miss.
lookup_cache(CacheTable, CacheKey, TTL) ->
    try
        case ets:lookup(CacheTable, CacheKey) of
            [{_, Allowed, Timestamp}] ->
                Now = erlang:system_time(second),
                case Now - Timestamp < TTL of
                    true -> {ok, Allowed};
                    false -> miss
                end;
            [] ->
                miss
        end
    catch
        _:_ -> miss
    end.

%% Store authorization result in cache
-spec store_cache(atom(), {binary(), binary()}, boolean()) -> ok.
store_cache(CacheTable, CacheKey, Allowed) ->
    Timestamp = erlang:system_time(second),
    try
        ets:insert(CacheTable, {CacheKey, Allowed, Timestamp}),
        ok
    catch
        _:_ -> ok
    end.

%% Make API call to check authorization
-spec call_api(binary(), binary(), binary()) ->
    {ok, boolean()} | {error, term()}.
call_api(FromJid, ToJid, LServer) ->
    ApiUrl = mod_group_filter_opt:api_url(LServer),
    Timeout = mod_group_filter_opt:timeout(LServer),

    RequestBody = jiffy:encode(#{
        <<"from">> => FromJid,
        <<"to">> => ToJid
    }),

    Headers = [{<<"Content-Type">>, <<"application/json">>}],

    %% Use hackney with connection pooling
    Options = [
        {pool, mod_group_filter_pool},
        {recv_timeout, Timeout},
        {connect_timeout, Timeout}
    ],

    case hackney:post(ApiUrl, Headers, RequestBody, Options) of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, Body} ->
                    try
                        #{<<"allowed">> := Allowed} = jiffy:decode(Body, [return_maps]),
                        {ok, Allowed}
                    catch
                        _:_ ->
                            {error, invalid_response_format}
                    end;
                {error, Reason} ->
                    {error, {body_error, Reason}}
            end;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

depends(_Host, _Opts) ->
    [].

mod_opt_type(api_url) ->
    econf:url();
mod_opt_type(timeout) ->
    econf:timeout(millisecond);
mod_opt_type(cache_ttl) ->
    econf:timeout(second).

mod_options(_) ->
    [{api_url, <<"http://localhost:3000/check-messaging-permission">>},
     {timeout, 500},
     {cache_ttl, 30}].

mod_doc() ->
    #{desc =>
          ?T("This module filters user-to-user messages based on dynamic "
             "group membership. It calls an external API to check if the "
             "sender is authorized to message the recipient based on their "
             "current group memberships. Messages are blocked if users are "
             "not in the same group, or if the API is unavailable (fail-secure)."),
      opts =>
          [{api_url,
            #{value => ?T("URL"),
              desc =>
                  ?T("The URL of the external API endpoint that checks "
                     "messaging permissions. The API should accept POST "
                     "requests with JSON body containing 'from' and 'to' "
                     "JIDs, and return JSON with 'allowed' boolean. "
                     "Default: http://localhost:3000/check-messaging-permission")}},
           {timeout,
            #{value => ?T("timeout()"),
              desc =>
                  ?T("Timeout in milliseconds for API requests. "
                     "The default value is '500' (500ms).")}},
           {cache_ttl,
            #{value => ?T("timeout()"),
              desc =>
                  ?T("Time-to-live for cached authorization results in seconds. "
                     "Set to 0 to disable caching. Caching reduces API load and "
                     "improves performance by storing authorization decisions. "
                     "The default value is '30' (30 seconds).")}}]}.
