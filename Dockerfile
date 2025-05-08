FROM eclipse-temurin:17

# Install packages
RUN apt-get update && \
    apt-get install -y make git gzip gnupg

# Install Apptainer
# This is to support the default 'buildah' builder which uses 'apptainer run ...'
ARG APPTAINER_VERSION=1.3.1
RUN curl -sSL "https://github.com/apptainer/apptainer/releases/download/v${APPTAINER_VERSION}/apptainer_${APPTAINER_VERSION}_amd64.deb" -o /tmp/apptainer.deb && \
    apt-get update && \
    # Install apptainer and its dependencies. apt-get can resolve dependencies for local .deb files.
    # squashfs-tools is commonly needed/recommended for Apptainer's full functionality.
    apt-get install -y --no-install-recommends /tmp/apptainer.deb squashfs-tools && \
    rm /tmp/apptainer.deb

# Install sbt
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt

# Install yq
RUN curl -sSL https://github.com/mikefarah/yq/releases/download/v4.44.2/yq_linux_386 > /usr/bin/yq && \
    chmod +x /usr/bin/yq

# Run SBT once so that all libraries are downloaded
# Avoid running sbt from /
WORKDIR /app
RUN sbt exit

# Get sources
WORKDIR /app
COPY . /app/viash/
WORKDIR /app/viash

# Build, package, install
RUN ./configure
RUN make bin/viash
RUN make install
RUN make tools
