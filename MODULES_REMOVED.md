# Removed Modules - Minimal ejabberd Build

This document lists modules and features **NOT included** in the minimal ejabberd configuration (`ejabberd-minimal.yml`). This configuration is optimized for MUC + OMEMO with custom authentication.

## Modules Excluded

### Roster Management
- `mod_roster` - Contact lists/rosters
- `mod_shared_roster` - Shared group rosters
- `mod_shared_roster_ldap` - LDAP-based shared rosters

**Impact**: No contact lists, no roster groups. Users connect directly to MUC rooms.

### Offline Messages
- `mod_offline` - Store messages for offline users

**Impact**: No traditional offline message queue. Use `mod_mam` (included) for message history instead.

### User Registration
- `mod_register` - In-band user registration
- `mod_register_web` - Web-based registration

**Impact**: No self-service registration. Users created via custom auth API.

### Privacy & Blocking
- `mod_privacy` - Privacy lists (XEP-0016)
- `mod_blocking` - Simple blocking (XEP-0191)
- `mod_block_strangers` - Block messages from non-roster

**Impact**: No client-side blocking. Use `mod_group_filter` (included) for server-side filtering.

### vCards & Avatars
- `mod_vcard` - vCard support
- `mod_vcard_xupdate` - vCard avatar updates
- `mod_avatar` - Avatar management

**Impact**: No user profile information or avatars.

### File Transfer
- `mod_http_upload` - HTTP file upload (XEP-0363)
- `mod_http_upload_quota` - Upload quotas
- `mod_proxy65` - SOCKS5 proxy for file transfer

**Impact**: No file transfers. Consider external file sharing service.

### Presence & Activity
- `mod_last` - Last activity (XEP-0012)
- `mod_ping` - XMPP ping
- `mod_time` - Entity time
- `mod_version` - Software version

**Impact**: No presence timestamps, no keepalive pings.

### Private Storage
- `mod_private` - Private XML storage (XEP-0049)

**Impact**: No client-side data storage. Bookmarks stored via PubSub (included).

### Push Notifications
- `mod_push` - Push notifications (XEP-0357)
- `mod_push_keepalive` - Push keepalive

**Impact**: No mobile push notifications.

### Server-to-Server (S2S)
- `mod_s2s_dialback` - Server dialback
- `mod_s2s_bidi` - Bidirectional S2S

**Impact**: No federation with other XMPP servers. Local-only.

### Web/HTTP Features
- `mod_bosh` - BOSH connections (XEP-0124)
- `mod_http_api` - HTTP API
- `mod_http_fileserver` - Static file server
- `mod_conversejs` - Converse.js integration
- `ejabberd_http_ws` - WebSocket connections

**Impact**: No web clients. TCP/TLS connections only.

### Administration
- `mod_admin_extra` - Extra admin commands
- `mod_admin_update_sql` - SQL schema updates
- `mod_adhoc` - Ad-hoc commands (XEP-0050)
- `mod_configure` - Server configuration
- `ejabberd_web_admin` - Web admin interface

**Impact**: No web admin panel. Use `ejabberdctl` CLI only.

### Other Protocols
- `mod_mqtt` - MQTT protocol
- `mod_mqtt_bridge` - MQTT bridge
- `mod_sip` - SIP gateway
- `mod_matrix_gw` - Matrix gateway
- `mod_legacy_auth` - Legacy auth (XEP-0078)

**Impact**: XMPP only, no protocol bridges.

### Advanced Features
- `mod_mix` - MIX protocol (XEP-0369)
- `mod_multicast` - Multicast messages
- `mod_delegation` - Namespace delegation
- `mod_privilege` - Privileged entity
- `mod_stats` - Statistics
- `mod_metrics` - Prometheus metrics
- `mod_announce` - Server announcements
- `mod_muc_log` - MUC conversation logging (to disk)
- `mod_fail2ban` - Failed auth tracking
- `mod_antispam` - Anti-spam features

**Impact**: Basic feature set only.

## Build-Time Exclusions (Optional)

When running `./configure`, these can be disabled to reduce binary size:

```bash
./configure \
  --disable-stun      # No STUN/TURN server
  --disable-sip       # No SIP support
  --disable-mysql     # No MySQL backend
  --disable-pgsql     # No PostgreSQL backend
  --disable-sqlite    # No SQLite backend
  --disable-odbc      # No ODBC support
  --disable-redis     # No Redis backend
  --disable-pam       # No PAM auth
  --disable-zlib      # No stream compression
```

## What IS Included

### Custom Modules
- `ejabberd_auth_session` - API-based authentication
- `mod_group_filter` - Group-based message filtering

### MUC
- `mod_muc` - Multi-User Chat rooms

### OMEMO Support
- `mod_pubsub` (with `pep` plugin) - Device lists & key bundles
- `mod_caps` - Entity capabilities
- `mod_carboncopy` - Message carbons (multi-device)
- `mod_mam` - Message archive

### Core
- `mod_disco` - Service discovery

## Re-enabling Features

To add features back, edit `ejabberd-minimal.yml` and add module to `modules:` section. See `ejabberd.yml.example` for configuration options.

Example - Add vCards:
```yaml
modules:
  # ... existing modules ...
  mod_vcard: {}
```

## Database Backend

Current config uses **Mnesia** (built-in). For production with many users, consider adding SQL backend:

1. Uncomment in configure: `--enable-pgsql` or `--enable-mysql`
2. Add to `ejabberd-minimal.yml`:
```yaml
sql_type: pgsql
sql_server: "localhost"
sql_database: "ejabberd"
sql_username: "ejabberd"
sql_password: "password"

modules:
  mod_mam:
    db_type: sql  # Use SQL instead of Mnesia
```

## Security Notes

Excluded features that may have been security concerns:
- No S2S = no federation attacks
- No file upload = no malware uploads
- No web admin = reduced attack surface
- No registration = no spam accounts
- Custom auth only = controlled user creation

Remaining attack vectors:
- C2S client connections (use TLS)
- MUC room creation (restricted via `access_create`)
- API endpoints (secure your backend)
