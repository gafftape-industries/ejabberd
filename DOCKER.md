# ejabberd Minimal Docker Image

This is a minimal ejabberd Docker image for Gafftape Industries, built on Alpine Linux for ARM64 architecture.

## Features

- **MUC (Multi-User Chat)**: Full group chat support
- **OMEMO Encryption**: End-to-end encryption via XEP-0384
- **Custom Authentication**: `ejabberd_auth_session` for session-based auth via API
- **Message Filtering**: `mod_group_filter` for dynamic group-based message filtering
- **Minimal Footprint**: Only essential features enabled

## Excluded Features

The following features are disabled to minimize the image size:
- STUN
- SIP
- MySQL/PostgreSQL/SQLite/Redis databases
- PAM authentication

## Building the Image

### Prerequisites

- Docker with BuildKit support
- Docker Buildx (for multi-platform builds)

### Build for ARM64

```bash
# Make the build script executable
chmod +x docker-build.sh

# Build the image
./docker-build.sh
```

Or manually:

```bash
docker buildx build --platform linux/arm64 -t ejabberd-minimal:latest .
```

### Build for other platforms

```bash
# For AMD64
PLATFORM=linux/amd64 ./docker-build.sh

# For multi-platform
docker buildx build --platform linux/arm64,linux/amd64 -t ejabberd-minimal:latest .
```

## Running the Container

### Basic Usage

```bash
docker run -d \
  --name ejabberd \
  -p 5222:5222 \
  -p 5280:5280 \
  ejabberd-minimal:latest
```

### With Persistent Storage

```bash
docker run -d \
  --name ejabberd \
  -p 5222:5222 \
  -p 5280:5280 \
  -v $(pwd)/conf:/opt/ejabberd/conf \
  -v $(pwd)/database:/opt/ejabberd/database \
  -v $(pwd)/logs:/opt/ejabberd/logs \
  ejabberd-minimal:latest
```

### With Custom Configuration

```bash
# Copy your config to the host
mkdir -p conf
cp ejabberd-minimal.yml conf/ejabberd.yml

# Edit the configuration
nano conf/ejabberd.yml

# Run with mounted config
docker run -d \
  --name ejabberd \
  -p 5222:5222 \
  -p 5280:5280 \
  -v $(pwd)/conf:/opt/ejabberd/conf \
  -v $(pwd)/database:/opt/ejabberd/database \
  -v $(pwd)/logs:/opt/ejabberd/logs \
  ejabberd-minimal:latest
```

## Environment Variables

The container supports the following environment variables:

- `ERLANG_NODE_ARG`: Erlang node name (default: `ejabberd@localhost`)
- `CONFIG_DIR`: Configuration directory (default: `/opt/ejabberd/conf`)
- `LOGS_DIR`: Logs directory (default: `/opt/ejabberd/logs`)
- `SPOOL_DIR`: Database/spool directory (default: `/opt/ejabberd/database`)

## Exposed Ports

- `5222`: XMPP client connections (C2S)
- `5269`: XMPP server connections (S2S) - not used in minimal config
- `5280`: HTTP/WebSocket endpoint

## Health Check

The container includes a health check that runs `ejabberdctl status` every minute.

```bash
# Check container health
docker inspect --format='{{.State.Health.Status}}' ejabberd
```

## Management Commands

```bash
# Check ejabberd status
docker exec ejabberd ejabberdctl status

# List connected users
docker exec ejabberd ejabberdctl connected_users

# List MUC rooms
docker exec ejabberd ejabberdctl muc_online_rooms global

# View logs
docker logs ejabberd
docker exec ejabberd tail -f /opt/ejabberd/logs/ejabberd.log
```

## Custom Modules

### ejabberd_auth_session

Provides session-based authentication via external API.

**API Endpoints:**
- `POST /validate` - Validate session and get room assignment
- `POST /register` - Register session
- `POST /unregister` - Unregister session

**Configuration:**
```yaml
auth_method: session

modules:
  ejabberd_auth_session:
    api_url: "http://localhost:3000"
    timeout: 2000
    muc_domain: "conference.localhost"
```

### mod_group_filter

Filters messages based on dynamic group membership via API.

**API Endpoint:**
- `POST /check-messaging-permission` - Check if users can message each other

**Configuration:**
```yaml
modules:
  mod_group_filter:
    api_url: "http://localhost:3000/check-messaging-permission"
    timeout: 500
    cache_ttl: 30
```

## Troubleshooting

### Container won't start

Check logs:
```bash
docker logs ejabberd
```

### Custom modules not loading

Verify modules are compiled:
```bash
docker exec ejabberd ls -la /opt/ejabberd/lib/ejabberd-*/ebin/ejabberd_auth_session*.beam
docker exec ejabberd ls -la /opt/ejabberd/lib/ejabberd-*/ebin/mod_group_filter*.beam
```

### Can't connect to external API

Make sure the container can reach your API:
```bash
docker exec ejabberd curl -v http://your-api:3000/validate
```

If using `localhost`, you may need to use `host.docker.internal` instead:
```yaml
api_url: "http://host.docker.internal:3000"
```

## Development

### Rebuild after code changes

```bash
# Rebuild the image
./docker-build.sh

# Stop and remove old container
docker stop ejabberd
docker rm ejabberd

# Start new container
docker run -d --name ejabberd -p 5222:5222 -p 5280:5280 ejabberd-minimal:latest
```

### Enter the container

```bash
docker exec -it ejabberd sh
```

### Debug mode

Run in foreground to see logs:
```bash
docker run -it --rm -p 5222:5222 -p 5280:5280 ejabberd-minimal:latest foreground
```

## Production Deployment

For production, consider:

1. **Use docker-compose** for easier management
2. **Mount volumes** for persistent data
3. **Configure TLS** with proper certificates
4. **Set resource limits** to prevent OOM
5. **Use secrets** for sensitive configuration
6. **Enable monitoring** with health checks
7. **Configure log rotation** to manage disk space

Example docker-compose.yml:
```yaml
version: '3.8'

services:
  ejabberd:
    image: ejabberd-minimal:latest
    ports:
      - "5222:5222"
      - "5280:5280"
    volumes:
      - ./conf:/opt/ejabberd/conf
      - ./database:/opt/ejabberd/database
      - ./logs:/opt/ejabberd/logs
    environment:
      - ERLANG_NODE_ARG=ejabberd@example.com
    restart: unless-stopped
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 1G
        reservations:
          cpus: '0.5'
          memory: 256M
```

## License

See [COPYING](COPYING) for license information.
