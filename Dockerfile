# Build stage
FROM haskell:9.4 as builder

WORKDIR /app

# Copy only the cabal file for dependency resolution
COPY mock.cabal ./

# Install dependencies
RUN cabal update && \
    cabal build --only-dependencies

# Copy the rest of the source code
COPY . .

# Build the application
RUN cabal build && \
    cabal install --installdir=/app/bin --overwrite-policy=always

# Runtime stage
FROM ubuntu:22.04

WORKDIR /app

# Install runtime dependencies, SSH server, and Node.js for Mountebank
RUN apt-get update && \
    apt-get install -y \
    libgmp10 \
    openssh-server \
    netcat \
    curl \
    nodejs \
    npm && \
    rm -rf /var/lib/apt/lists/* && \
    mkdir -p /var/run/sshd && \
    mkdir -p /root/.ssh && \
    chmod 700 /root/.ssh && \
    # Install Mountebank
    npm install -g mountebank@2.8.1

# Copy the binary from the builder stage
COPY --from=builder /app/bin/beegfs-ctl /usr/local/bin/

# Create directory for Mountebank imposters
RUN mkdir -p /imposters

# Copy Mountebank imposters
COPY imposters/ /imposters/

# SSH configuration
RUN sed -i 's/#PermitRootLogin.*/PermitRootLogin yes/' /etc/ssh/sshd_config && \
    sed -i 's/#PasswordAuthentication.*/PasswordAuthentication no/' /etc/ssh/sshd_config && \
    sed -i 's/#PubkeyAuthentication.*/PubkeyAuthentication yes/' /etc/ssh/sshd_config

# Expose ports
EXPOSE 22 2525 8080

# Create startup script
COPY <<'EOF' /start.sh
#!/bin/bash
# Start SSH daemon
/usr/sbin/sshd

# Start Mountebank with imposters
mb --configfile /imposters/imposters.ejs --allowInjection &

# Keep container running
exec tail -f /dev/null
EOF

RUN chmod +x /start.sh

# Start services
CMD ["/start.sh"] 