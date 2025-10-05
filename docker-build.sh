#!/bin/bash
#
# Build script for ejabberd minimal Docker image
# Platform: linux/arm64
#

set -e

# Configuration
IMAGE_NAME="ejabberd-minimal"
TAG="${TAG:-latest}"
PLATFORM="${PLATFORM:-linux/arm64}"

echo "Building ejabberd minimal Docker image..."
echo "Image: $IMAGE_NAME:$TAG"
echo "Platform: $PLATFORM"
echo

# Build the image
docker buildx build \
    --platform "$PLATFORM" \
    --tag "$IMAGE_NAME:$TAG" \
    --load \
    .

echo
echo "✅ Build complete!"
echo

# Prepare volume directories
echo "Preparing volume directories..."
mkdir -p conf database logs

# Copy config file if it doesn't exist
if [ ! -f conf/ejabberd.yml ]; then
    echo "Copying default configuration to conf/ejabberd.yml..."
    cp ejabberd-minimal.yml conf/ejabberd.yml
fi

echo
echo "To run the container:"
echo "  docker run -d \\"
echo "    --name ejabberd \\"
echo "    -p 5222:5222 \\"
echo "    -p 5280:5280 \\"
echo "    -v \$(pwd)/conf:/opt/ejabberd/conf \\"
echo "    -v \$(pwd)/database:/opt/ejabberd/database \\"
echo "    -v \$(pwd)/logs:/opt/ejabberd/logs \\"
echo "    $IMAGE_NAME:$TAG"
echo
echo "To check status:"
echo "  docker exec ejabberd ejabberdctl status"
echo
