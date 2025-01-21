# Build stage
FROM haskell:9.4 AS builder

WORKDIR /app

# Copy only files needed for dependency resolution
COPY mock.cabal cabal.project* stack.yaml* ./

# Install dependencies in a separate layer
RUN cabal update && \
    cabal build --only-dependencies

# Copy source code files
COPY src/ src/
COPY test/ test/
COPY src-bin/ src-bin/
COPY imposters/ imposters/
COPY scripts/ scripts/

# Build the application
RUN cabal build && \
    cabal install --installdir=/app/bin --overwrite-policy=always

# Runtime stage
FROM ubuntu:22.04

WORKDIR /app

# Install runtime dependencies in a single layer
RUN apt-get update && \
    apt-get install -y \
    libgmp10 \
    openssh-server \
    netcat \
    curl \
    nodejs \
    npm \
    jq && \
    rm -rf /var/lib/apt/lists/* && \
    # Create required directories
    mkdir -p /var/run/sshd /root/.ssh /imposters && \
    chmod 700 /root/.ssh && \
    # Configure SSH
    sed -i 's/#PermitRootLogin.*/PermitRootLogin yes/' /etc/ssh/sshd_config && \
    sed -i 's/#PasswordAuthentication.*/PasswordAuthentication no/' /etc/ssh/sshd_config && \
    sed -i 's/#PubkeyAuthentication.*/PubkeyAuthentication yes/' /etc/ssh/sshd_config && \
    # Install Mountebank
    npm install -g mountebank@2.8.1

# Copy the binaries from the builder stage
COPY --from=builder /app/bin/beegfs-ctl /usr/local/bin/
COPY --from=builder /app/bin/zfs /usr/local/bin/

# Copy Mountebank imposters last since they might change frequently
COPY imposters/ /imposters/

# Expose ports
EXPOSE 22 2525 8080 