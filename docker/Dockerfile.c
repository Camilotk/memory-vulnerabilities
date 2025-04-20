FROM gcc:latest

WORKDIR /app

# Install necessary tools
RUN apt-get update && apt-get install -y \
    make \
    valgrind \
    gdb \
    && rm -rf /var/lib/apt/lists/*

# Copy source code
COPY . /app/

# Create build directory and build all C examples
RUN mkdir -p _build && make all-c

# Default command
CMD ["/bin/bash"]
