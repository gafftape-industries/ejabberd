#' Minimal ejabberd Docker image for Gafftape Industries
#' Includes: MUC, OMEMO, ejabberd_auth_session, mod_group_filter
#' Platform: linux/arm64

################################################################################
#' Define default build variables
ARG OTP_VSN='27.3.4.3'
ARG UID='9000'
ARG USER='ejabberd'
ARG HOME="opt/$USER"
ARG BUILD_DIR="/$USER"

################################################################################
#' Build ejabberd from source
FROM docker.io/erlang:${OTP_VSN}-alpine AS builder

# Install build dependencies (minimal set)
RUN apk -U add --no-cache \
        autoconf \
        automake \
        bash \
        build-base \
        curl \
        expat-dev \
        file \
        gd-dev \
        git \
        jpeg-dev \
        libpng-dev \
        libwebp-dev \
        openssl \
        openssl-dev \
        sqlite-dev \
        yaml-dev \
        zlib-dev

ARG BUILD_DIR
WORKDIR $BUILD_DIR

# Copy source code including custom modules
COPY . $BUILD_DIR/

# Generate configure script if needed
RUN if [ ! -f ./configure ]; then ./autogen.sh; fi

# Configure with minimal feature set
# Enable: SQLite (for MAM)
# Disable: STUN, SIP, MySQL, PostgreSQL, Redis, PAM
RUN ./configure \
    --with-rebar=rebar3 \
    --enable-sqlite \
    --disable-stun \
    --disable-sip \
    --disable-mysql \
    --disable-pgsql \
    --disable-redis \
    --disable-pam

# Install dependencies and build production release
RUN make deps && make rel

# Verify custom modules were compiled
RUN ls -la _build/prod/rel/ejabberd/lib/ejabberd-*/ebin/ejabberd_auth_session*.beam || \
    (echo "ERROR: ejabberd_auth_session not compiled!" && exit 1)
RUN ls -la _build/prod/rel/ejabberd/lib/ejabberd-*/ebin/mod_group_filter*.beam || \
    (echo "ERROR: mod_group_filter not compiled!" && exit 1)

################################################################################
#' Prepare runtime filesystem
FROM builder AS runtime-prep

ARG BUILD_DIR
ARG HOME
WORKDIR /rootfs

# Create directory structure
RUN mkdir -p $HOME/conf $HOME/database $HOME/logs $HOME/upload

# Copy ejabberd release
RUN cp -r $BUILD_DIR/_build/prod/rel/ejabberd/* $HOME/

# Copy minimal configuration
RUN cp $BUILD_DIR/ejabberd-minimal.yml $HOME/conf/ejabberd.yml

# Download CA certificates
RUN wget -O "$HOME/conf/cacert.pem" 'https://curl.se/ca/cacert.pem' || \
    curl -L -o "$HOME/conf/cacert.pem" 'https://curl.se/ca/cacert.pem'

# Generate self-signed certificate for localhost
RUN openssl req -x509 \
        -batch \
        -nodes \
        -newkey rsa:4096 \
        -keyout $HOME/conf/server.pem \
        -out $HOME/conf/server.pem \
        -days 3650 \
        -subj "/CN=localhost"

# Create ejabberdctl wrapper script
RUN mkdir -p usr/local/bin && \
    echo '#!/bin/sh' > usr/local/bin/ejabberdctl && \
    echo '[ -z $ERLANG_NODE_ARG ] && export ERLANG_NODE_ARG=ejabberd@localhost' >> usr/local/bin/ejabberdctl && \
    echo 'export CONFIG_DIR=/'$HOME'/conf' >> usr/local/bin/ejabberdctl && \
    echo 'export LOGS_DIR=/'$HOME'/logs' >> usr/local/bin/ejabberdctl && \
    echo 'export SPOOL_DIR=/'$HOME'/database' >> usr/local/bin/ejabberdctl && \
    echo 'exec /'$HOME'/bin/ejabberdctl "$@"' >> usr/local/bin/ejabberdctl && \
    chmod +x usr/local/bin/ejabberdctl

# Determine runtime dependencies
RUN scanelf --needed --nobanner --format '%n#p' --recursive $HOME \
        | tr ',' '\n' \
        | sort -u \
        | awk 'NF > 0 { if (system("[ -e /'$HOME'/" $1 " ]") != 0) print "so:" $1 }' \
        | sed -e "s|so:libc.so|so:libc.musl-$(uname -m).so.1|" \
        > /tmp/runDeps

################################################################################
#' Create minimal runtime image
FROM docker.io/alpine:latest AS runtime

# Install only necessary runtime dependencies
COPY --from=runtime-prep /tmp/runDeps /tmp/runDeps
RUN apk -U upgrade --available --no-cache && \
    apk add --no-cache \
        $(cat /tmp/runDeps) \
        libcap \
        curl \
        tini && \
    rm /tmp/runDeps

# Create ejabberd user
ARG USER
ARG UID
ARG HOME
RUN addgroup $USER -g $UID && \
    adduser -s /sbin/nologin -D -u $UID -h /$HOME -G $USER $USER

# Copy ejabberd installation from prep stage
COPY --from=runtime-prep --chown=$UID:$UID /rootfs/$HOME /$HOME
COPY --from=runtime-prep /rootfs/usr/local/bin/ejabberdctl /usr/local/bin/ejabberdctl

# Set capabilities for beam.smp to bind to privileged ports
RUN setcap 'cap_net_bind_service=+ep' $(find /$HOME -name beam.smp)

################################################################################
#' Final production image
FROM scratch

ARG USER
ARG HOME

COPY --from=runtime / /

# Health check
HEALTHCHECK \
    --interval=1m \
    --timeout=5s \
    --start-period=10s \
    --retries=10 \
    CMD ejabberdctl status

WORKDIR /$HOME
USER $USER

# Expose ports
# 5222: XMPP client connections (C2S)
# 5269: XMPP server connections (S2S) - not used in minimal config
# 5280: HTTP/WebSocket
EXPOSE 5222 5269 5280

# Volume for persistence
VOLUME ["/$HOME/conf", "/$HOME/database", "/$HOME/logs"]

# Start ejabberd in foreground mode (required for containers)
ENTRYPOINT ["/sbin/tini", "--", "ejabberdctl"]
CMD ["foreground"]
