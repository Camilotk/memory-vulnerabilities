# Root Makefile for memory-vulnerabilities project

# Define build directory
BUILD_DIR = _build

# Define C compiler and flags
CC = gcc
# Basic flags
CFLAGS = -Wall -Wextra
# Always use unsafe flags to demonstrate vulnerabilities
UNSAFE_FLAGS = -fno-stack-protector -z execstack -no-pie -D_FORTIFY_SOURCE=0
LDFLAGS = -pthread

# Default target
all: all-c all-erlang

# Build all C examples
all-c: spatial-c temporal-c

# Build Erlang examples
all-erlang: spatial-erlang temporal-erlang

# Create build directory structure
$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/buffer-overflow
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/out-of-bounds
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/improper-memory
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/stack-vulnerabilities
	@mkdir -p $(BUILD_DIR)/c-examples/temporal/use-after-free
	@mkdir -p $(BUILD_DIR)/c-examples/temporal/double-free
	@mkdir -p $(BUILD_DIR)/c-examples/temporal/memory-leaks
	@mkdir -p $(BUILD_DIR)/c-examples/temporal/race-conditions
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/buffer-safety
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/bounds-safety
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/memory-safety
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/stack-safety
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/reference-safety
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/garbage-collection
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/leak-prevention
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/concurrency-safety

# Spatial memory vulnerabilities in C
spatial-c: buffer-overflow out-of-bounds improper-memory stack-vulnerability

# Temporal memory vulnerabilities in C
temporal-c: use-after-free double-free memory-leaks race-conditions

# Spatial memory safety in Erlang
spatial-erlang: buffer-safety bounds-safety memory-safety stack-safety

# Temporal memory safety in Erlang
temporal-erlang: reference-safety garbage-collection leak-prevention concurrency-safety

# Individual C examples
buffer-overflow: $(BUILD_DIR)
	@echo "Building buffer overflow example..."
	# Build with unsafe flags to demonstrate vulnerability
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/spatial/buffer-overflow/buffer_overflow c-examples/spatial/buffer-overflow/buffer_overflow.c
	# Copy password.txt to build directory
	cp c-examples/spatial/buffer-overflow/password.txt $(BUILD_DIR)/c-examples/spatial/buffer-overflow/
	cp c-examples/spatial/buffer-overflow/*.py $(BUILD_DIR)/c-examples/spatial/buffer-overflow/

out-of-bounds: $(BUILD_DIR)
	@echo "Building out-of-bounds example..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/spatial/out-of-bounds/out-of-bounds c-examples/spatial/out-of-bounds/out-of-bounds.c

improper-memory: $(BUILD_DIR)
	@echo "Building improper memory handling example..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/spatial/improper-memory/null-pointer c-examples/spatial/improper-memory/null-pointer.c

stack-vulnerability: $(BUILD_DIR)
	@echo "Building stack vulnerability example..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/spatial/stack-vulnerabilities/stack-overflow c-examples/spatial/stack-vulnerabilities/stack-overflow.c

use-after-free: $(BUILD_DIR)
	@echo "Building use-after-free example..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/temporal/use-after-free/use-after-free c-examples/temporal/use-after-free/use-after-free.c

double-free: $(BUILD_DIR)
	@echo "Building double-free example..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/temporal/double-free/double-free c-examples/temporal/double-free/double-free.c

memory-leaks: $(BUILD_DIR)
	@echo "Building memory leaks example..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/temporal/memory-leaks/memory-leaks c-examples/temporal/memory-leaks/memory-leaks.c

race-conditions: $(BUILD_DIR)
	@echo "Building race conditions example..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) $(LDFLAGS) -o $(BUILD_DIR)/c-examples/temporal/race-conditions/race-conditions c-examples/temporal/race-conditions/race-conditions.c

# Individual Erlang examples (compile to .beam files)
buffer-safety: $(BUILD_DIR)
	@echo "Building buffer safety example..."
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/buffer-safety erlang-examples/spatial/buffer-safety/safe_buffer.erl
	cp erlang-examples/spatial/buffer-safety/password.txt $(BUILD_DIR)/erlang-examples/spatial/buffer-safety/

bounds-safety: $(BUILD_DIR)
	@echo "Building bounds safety example..."
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/bounds-safety erlang-examples/spatial/bounds-safety/array_example.erl

memory-safety: $(BUILD_DIR)
	@echo "Building memory safety example..."
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/memory-safety erlang-examples/spatial/memory-safety/memory_safety.erl

stack-safety: $(BUILD_DIR)
	@echo "Building stack safety example..."
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/stack-safety erlang-examples/spatial/stack-safety/recursion.erl

reference-safety: $(BUILD_DIR)
	@echo "Building reference safety example..."
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/reference-safety erlang-examples/temporal/reference-safety/memory_safety.erl

garbage-collection: $(BUILD_DIR)
	@echo "Building garbage collection example..."
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/garbage-collection erlang-examples/temporal/garbage-collection/memory_management.erl

leak-prevention: $(BUILD_DIR)
	@echo "Building leak prevention example..."
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/leak-prevention erlang-examples/temporal/leak-prevention/memory_management.erl

concurrency-safety: $(BUILD_DIR)
	@echo "Building concurrency safety example..."
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/concurrency-safety erlang-examples/temporal/concurrency-safety/concurrency.erl

# Build a specific example in debug mode with source code info (still using unsafe flags)
debug-buffer-overflow: $(BUILD_DIR)
	@echo "Building buffer overflow example in debug mode..."
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -g -o $(BUILD_DIR)/c-examples/spatial/buffer-overflow/buffer_overflow_debug c-examples/spatial/buffer-overflow/buffer_overflow.c
	cp c-examples/spatial/buffer-overflow/password.txt $(BUILD_DIR)/c-examples/spatial/buffer-overflow/

# Run tests
test: all
	@echo "Running tests..."
	cd test && ./run_tests.sh

# Create the source project structure
structure:
	@echo "Creating source project structure..."
	@mkdir -p c-examples/spatial/buffer-overflow
	@mkdir -p c-examples/spatial/out-of-bounds
	@mkdir -p c-examples/spatial/improper-memory
	@mkdir -p c-examples/spatial/stack-vulnerabilities
	@mkdir -p c-examples/temporal/use-after-free
	@mkdir -p c-examples/temporal/double-free
	@mkdir -p c-examples/temporal/memory-leaks
	@mkdir -p c-examples/temporal/race-conditions
	@mkdir -p erlang-examples/spatial/buffer-safety
	@mkdir -p erlang-examples/spatial/bounds-safety
	@mkdir -p erlang-examples/spatial/memory-safety
	@mkdir -p erlang-examples/spatial/stack-safety
	@mkdir -p erlang-examples/temporal/reference-safety
	@mkdir -p erlang-examples/temporal/garbage-collection
	@mkdir -p erlang-examples/temporal/leak-prevention
	@mkdir -p erlang-examples/temporal/concurrency-safety
	@mkdir -p docker
	@mkdir -p test/c-tests
	@mkdir -p test/erlang-tests

# Clean all built files
clean:
	@echo "Cleaning all build artifacts..."
	rm -rf $(BUILD_DIR)

# Documentation for the examples
docs:
	@echo "Generating documentation..."
	# Add documentation generation commands here

.PHONY: all all-c all-erlang spatial-c temporal-c spatial-erlang temporal-erlang \
        buffer-overflow out-of-bounds improper-memory stack-vulnerability \
        use-after-free double-free memory-leaks race-conditions \
        buffer-safety bounds-safety memory-safety stack-safety \
        reference-safety garbage-collection leak-prevention concurrency-safety \
        clean structure test docs debug-buffer-overflow
