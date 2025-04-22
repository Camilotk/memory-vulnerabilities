# Root Makefile for memory-vulnerabilities project

BUILD_DIR = _build
CC = gcc
CFLAGS = -std=c11
UNSAFE_FLAGS = -fno-stack-protector -z execstack -no-pie -D_FORTIFY_SOURCE=0
LDFLAGS = -pthread

# Top-level targets
all: all-c all-erlang
all-c: spatial-c temporal-c
all-erlang: spatial-erlang temporal-erlang

spatial-c: buffer-overflow out-of-bounds pointer-arithmetic
temporal-c: use-after-free memory-leaks

spatial-erlang: buffer-safety bounds-safety memory-safety stack-safety
temporal-erlang: reference-safety garbage-collection leak-prevention concurrency-safety

# C Spatial
buffer-overflow:
	@echo "Building buffer overflow..."
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/buffer-overflow
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/spatial/buffer-overflow/example c-examples/spatial/buffer-overflow/example.c
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -Wno-implicit-function-declaration -o $(BUILD_DIR)/c-examples/spatial/buffer-overflow/login c-examples/spatial/buffer-overflow/login.c
	cp c-examples/spatial/buffer-overflow/password.txt $(BUILD_DIR)/c-examples/spatial/buffer-overflow/
	cp c-examples/spatial/buffer-overflow/*.py $(BUILD_DIR)/c-examples/spatial/buffer-overflow/

out-of-bounds:
	@echo "Building out-of-bounds..."
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/out-of-bounds
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/spatial/out-of-bounds/out-of-bounds c-examples/spatial/out-of-bounds/out-of-bounds.c

pointer-arithmetic:
	@echo "Building pointer arithmetic..."
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/pointer-arithmetic
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/spatial/pointer-arithmetic/example c-examples/spatial/pointer-arithmetic/example.c

# C Temporal
use-after-free:
	@echo "Building use-after-free..."
	@mkdir -p $(BUILD_DIR)/c-examples/temporal/use-after-free
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/temporal/use-after-free/useradd c-examples/temporal/use-after-free/useradd.c
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/temporal/use-after-free/example c-examples/temporal/use-after-free/example.c
	cp c-examples/temporal/use-after-free/*.py $(BUILD_DIR)/c-examples/temporal/use-after-free/

memory-leaks:
	@echo "Building memory leaks..."
	@mkdir -p $(BUILD_DIR)/c-examples/temporal/memory-leaks
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/c-examples/temporal/memory-leaks/memory-leaks c-examples/temporal/memory-leaks/memory-leaks.c

# Erlang Spatial
buffer-safety:
	@echo "Building Erlang buffer safety..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/buffer-safety
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/buffer-safety erlang-examples/spatial/buffer-safety/login.erl
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/buffer-safety erlang-examples/spatial/buffer-safety/example.erl
	cp erlang-examples/spatial/buffer-safety/password.txt $(BUILD_DIR)/erlang-examples/spatial/buffer-safety/

bounds-safety:
	@echo "Building Erlang bounds safety..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/bounds-safety
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/bounds-safety erlang-examples/spatial/bounds-safety/array_example.erl

memory-safety:
	@echo "Building Erlang memory safety..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/memory-safety
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/memory-safety erlang-examples/spatial/memory-safety/memory_safety.erl

stack-safety:
	@echo "Building Erlang stack safety..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/spatial/stack-safety
	erlc -o $(BUILD_DIR)/erlang-examples/spatial/stack-safety erlang-examples/spatial/stack-safety/recursion.erl

# Erlang Temporal
reference-safety:
	@echo "Building Erlang reference safety..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/reference-safety
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/reference-safety erlang-examples/temporal/reference-safety/useradd.erl
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/reference-safety erlang-examples/temporal/reference-safety/example.erl

garbage-collection:
	@echo "Building Erlang garbage collection..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/garbage-collection
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/garbage-collection erlang-examples/temporal/garbage-collection/memory_management.erl

leak-prevention:
	@echo "Building Erlang leak prevention..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/leak-prevention
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/leak-prevention erlang-examples/temporal/leak-prevention/memory_management.erl

concurrency-safety:
	@echo "Building Erlang concurrency safety..."
	@mkdir -p $(BUILD_DIR)/erlang-examples/temporal/concurrency-safety
	erlc -o $(BUILD_DIR)/erlang-examples/temporal/concurrency-safety erlang-examples/temporal/concurrency-safety/concurrency.erl

# Utility
debug-buffer-overflow:
	@echo "Building debug buffer overflow..."
	@mkdir -p $(BUILD_DIR)/c-examples/spatial/buffer-overflow
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -g -o $(BUILD_DIR)/c-examples/spatial/buffer-overflow/buffer_overflow_debug c-examples/spatial/buffer-overflow/buffer_overflow.c
	cp c-examples/spatial/buffer-overflow/password.txt $(BUILD_DIR)/c-examples/spatial/buffer-overflow/

clean:
	@echo "Cleaning all build artifacts..."
	rm -rf $(BUILD_DIR)

.PHONY: all all-c all-erlang spatial-c temporal-c spatial-erlang temporal-erlang \
	buffer-overflow out-of-bounds pointer-arithmetic \
	use-after-free memory-leaks \
	buffer-safety bounds-safety memory-safety stack-safety \
	reference-safety garbage-collection leak-prevention concurrency-safety \
	debug-buffer-overflow clean structure test docs
