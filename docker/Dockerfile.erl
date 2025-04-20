FROM erlang:latest

WORKDIR /app

# Copy source code
COPY . /app/

# Create build directory and build all Erlang examples
RUN mkdir -p _build && make all-erlang

# Default command
CMD ["/bin/bash"]
