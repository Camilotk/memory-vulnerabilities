# Root Makefile for memory-vulnerabilities project

BUILD_DIR = _build
CC = gcc
CFLAGS = -std=c11
UNSAFE_FLAGS = -fno-stack-protector -z execstack -no-pie -D_FORTIFY_SOURCE=0
LDFLAGS = -pthread
JAVAC = javac
JAVA_FLAGS = 

# Top-level targets
all: all-c all-erlang all-pascal all-java

all-c: spatial-c temporal-c
all-erlang: spatial-erlang temporal-erlang
all-pascal: out-of-bounds
all-java: race-conditions

# C Spatial
spatial-c: buffer-overflow pointer-arithmetic
temporal-c: use-after-free memory-leaks

buffer-overflow:
	@echo "Building buffer overflow..."
	@mkdir -p $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow/example vulnerability-examples/spatial/buffer-overflow/example.c
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -Wno-implicit-function-declaration -o $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow/login vulnerability-examples/spatial/buffer-overflow/login.c
	cp vulnerability-examples/spatial/buffer-overflow/password.txt $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow/
	cp vulnerability-examples/spatial/buffer-overflow/*.py $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow/

pointer-arithmetic:
	@echo "Building pointer arithmetic..."
	@mkdir -p $(BUILD_DIR)/vulnerability-examples/spatial/pointer-arithmetic
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/vulnerability-examples/spatial/pointer-arithmetic/example vulnerability-examples/spatial/pointer-arithmetic/example.c

# Pascal Spatial
out-of-bounds:
	@echo "Building Pascal out-of-bounds example..."
	@mkdir -p $(BUILD_DIR)/vulnerability-examples/spatial/out-of-bounds
	fpc -Mobjfpc -Rintel -O2 -o$(BUILD_DIR)/vulnerability-examples/spatial/out-of-bounds/example vulnerability-examples/spatial/out-of-bounds/example.pas
	fpc -Mobjfpc -Rintel -O2 -o$(BUILD_DIR)/vulnerability-examples/spatial/out-of-bounds/cal vulnerability-examples/spatial/out-of-bounds/cal.pas
	cp vulnerability-examples/spatial/out-of-bounds/*.py $(BUILD_DIR)/vulnerability-examples/spatial/out-of-bounds/

# C Temporal
use-after-free:
	@echo "Building use-after-free..."
	@mkdir -p $(BUILD_DIR)/vulnerability-examples/temporal/use-after-free
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/vulnerability-examples/temporal/use-after-free/useradd vulnerability-examples/temporal/use-after-free/useradd.c
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/vulnerability-examples/temporal/use-after-free/example vulnerability-examples/temporal/use-after-free/example.c
	cp vulnerability-examples/temporal/use-after-free/*.py $(BUILD_DIR)/vulnerability-examples/temporal/use-after-free/

memory-leaks:
	@echo "Building memory leaks..."
	@mkdir -p $(BUILD_DIR)/vulnerability-examples/temporal/memory-leaks
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -o $(BUILD_DIR)/vulnerability-examples/temporal/memory-leaks/memory-leaks vulnerability-examples/temporal/memory-leaks/memory-leaks.c

# Java Temporal
race-conditions:
	@echo "Building Java race conditions example..."
	@mkdir -p $(BUILD_DIR)/vulnerability-examples/temporal/race-conditions
	$(JAVAC) $(JAVA_FLAGS) -d $(BUILD_DIR)/vulnerability-examples/temporal/race-conditions vulnerability-examples/temporal/race-conditions/Example.java

# Erlang Spatial
spatial-erlang: buffer-safety bounds-safety memory-safety stack-safety
temporal-erlang: reference-safety garbage-collection leak-prevention concurrency-safety

buffer-safety:
	@echo "Building Erlang buffer safety..."
	@mkdir -p $(BUILD_DIR)/safety-examples/spatial/buffer-safety
	erlc -o $(BUILD_DIR)/safety-examples/spatial/buffer-safety safety-examples/spatial/buffer-safety/login.erl
	erlc -o $(BUILD_DIR)/safety-examples/spatial/buffer-safety safety-examples/spatial/buffer-safety/example.erl
	cp safety-examples/spatial/buffer-safety/password.txt $(BUILD_DIR)/safety-examples/spatial/buffer-safety/

bounds-safety:
	@echo "Building Erlang bounds safety..."
	@mkdir -p $(BUILD_DIR)/safety-examples/spatial/bounds-safety
	erlc -o $(BUILD_DIR)/safety-examples/spatial/bounds-safety safety-examples/spatial/bounds-safety/array_example.erl

memory-safety:
	@echo "Building Erlang memory safety..."
	@mkdir -p $(BUILD_DIR)/safety-examples/spatial/memory-safety
	erlc -o $(BUILD_DIR)/safety-examples/spatial/memory-safety safety-examples/spatial/memory-safety/memory_safety.erl

stack-safety:
	@echo "Building Erlang stack safety..."
	@mkdir -p $(BUILD_DIR)/safety-examples/spatial/stack-safety
	erlc -o $(BUILD_DIR)/safety-examples/spatial/stack-safety safety-examples/spatial/stack-safety/recursion.erl

# Erlang Temporal
reference-safety:
	@echo "Building Erlang reference safety..."
	@mkdir -p $(BUILD_DIR)/safety-examples/temporal/reference-safety
	erlc -o $(BUILD_DIR)/safety-examples/temporal/reference-safety safety-examples/temporal/reference-safety/useradd.erl
	erlc -o $(BUILD_DIR)/safety-examples/temporal/reference-safety safety-examples/temporal/reference-safety/example.erl

garbage-collection:
	@echo "Building Erlang garbage collection..."
	@mkdir -p $(BUILD_DIR)/safety-examples/temporal/garbage-collection
	erlc -o $(BUILD_DIR)/safety-examples/temporal/garbage-collection safety-examples/temporal/garbage-collection/memory_management.erl

leak-prevention:
	@echo "Building Erlang leak prevention..."
	@mkdir -p $(BUILD_DIR)/safety-examples/temporal/leak-prevention
	erlc -o $(BUILD_DIR)/safety-examples/temporal/leak-prevention safety-examples/temporal/leak-prevention/memory_management.erl

concurrency-safety:
	@echo "Building Erlang concurrency safety..."
	@mkdir -p $(BUILD_DIR)/safety-examples/temporal/concurrency-safety
	erlc -o $(BUILD_DIR)/safety-examples/temporal/concurrency-safety safety-examples/temporal/concurrency-safety/concurrency.erl

# Utility
debug-buffer-overflow:
	@echo "Building debug buffer overflow..."
	@mkdir -p $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow
	$(CC) $(CFLAGS) $(UNSAFE_FLAGS) -g -o $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow/buffer_overflow_debug vulnerability-examples/spatial/buffer-overflow/buffer_overflow.c
	cp vulnerability-examples/spatial/buffer-overflow/password.txt $(BUILD_DIR)/vulnerability-examples/spatial/buffer-overflow/

clean:
	@echo "Cleaning all build artifacts..."
	rm -rf $(BUILD_DIR)

.PHONY: all all-c all-erlang all-pascal all-java \
	spatial-c temporal-c spatial-erlang temporal-erlang \
	buffer-overflow pointer-arithmetic \
	use-after-free memory-leaks race-conditions \
	buffer-safety bounds-safety memory-safety stack-safety \
	reference-safety garbage-collection leak-prevention concurrency-safety \
	out-of-bounds debug-buffer-overflow clean
