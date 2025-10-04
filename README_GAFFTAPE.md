# Gafftape Industries - ejabberd Custom Modules

Custom ejabberd modules for group-based messaging and session authentication.

## mod_group_filter

Blocks client-to-client messages unless users are in the same group (per API). Fail-secure: API failures block messages.

**Config:**
```yaml
modules:
  mod_group_filter:
    api_url: "http://localhost:3000/check-messaging-permission"
    timeout: 500        # milliseconds
    cache_ttl: 30       # seconds (0 = disabled)
```

**API:** `POST {api_url}`
```json
// Request
{"from": "alice@example.com", "to": "bob@example.com"}

// Response
{"allowed": true}  // 200 OK = deliver, false = block, non-200 = block
```

---

## mod_session_auth

Session-based auth with API-assigned MUC rooms. Terminates session on any error.

**Config:**
```yaml
auth_method: mod_session_auth

modules:
  mod_session_auth:
    api_url: "http://localhost:3000"
    timeout: 5000                        # milliseconds
    muc_domain: "conference.example.com"
```

**API Endpoints:**

`POST {api_url}/validate`
```json
{"sessionID": "abc123", "user": "alice"}
// → 200 OK: {"room": "lobby"}
```

`POST {api_url}/register`
```json
{"sessionID": "abc123", "jid": "alice@example.com"}
// → 200 OK
```

`POST {api_url}/unregister`
```json
{"sessionID": "abc123", "jid": "alice@example.com"}
// → 200 OK
```

**Flow:** Client authenticates with username + sessionID (as password) → API validates & returns room name → room sanitized (`a-z0-9_-`, max 64 chars) → user registered → auto-joined to `{room}@{muc_domain}/{username}`. Any failure terminates session.

---

## Combined Example

```yaml
auth_method: mod_session_auth

modules:
  mod_session_auth:
    api_url: "https://api.example.com/auth"
    timeout: 2000
    muc_domain: "conference.example.com"

  mod_group_filter:
    api_url: "https://api.example.com/check-messaging-permission"
    timeout: 500
    cache_ttl: 30
```

---

## Security

Both modules are fail-secure: errors block operations rather than allowing them. Room names sanitized, sessions isolated in ETS, JIDs validated before API calls.

## Development

**Build:** Standard ejabberd build (`./autogen.sh && ./configure && make`)
**Test:** Configure in `ejabberd.yml`, start with `ejabberdctl start`, monitor logs
**Deps:** hackney (HTTP), jiffy (JSON) - both included with ejabberd
**Logs:** `tail -f /var/log/ejabberd/ejabberd.log`
**License:** GPL v2 (same as ejabberd)
