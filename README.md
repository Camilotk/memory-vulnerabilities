# Memory Vulnerabilities: C and Erlang Comparison

This repository contains code examples from the research paper "Memory Vulnerabilities: A Comparative Analysis in C and Erlang". It demonstrates common memory vulnerabilities in C and how they are prevented by design in Erlang.

## Repository Structure

The repository is organized by vulnerability type, with matching implementations in C (vulnerable) and Erlang (safe):

```
memory-vulnerabilities/
├── c-examples/          # Demonstrates memory vulnerabilities in C
│   ├── spatial/         # Spatial memory vulnerabilities
│   └── temporal/        # Temporal memory vulnerabilities
├── erlang-examples/     # Demonstrates memory safety in Erlang
│   ├── spatial/         # Protection against spatial memory issues
│   └── temporal/        # Protection against temporal memory issues
└── _build/              # Compiled binaries and .beam files
    ├── c-examples/      # Compiled C executables
    └── erlang-examples/ # Compiled Erlang modules
```

## Vulnerability Categories

### Spatial Memory Vulnerabilities
- Buffer Overflow/Underflow
- Out-of-Bounds Read/Write
- Improper Memory Handling
- Stack-Specific Vulnerabilities

### Temporal Memory Vulnerabilities
- Use-After-Free
- Double Free
- Memory Leaks
- Race Conditions in Memory Operations

## Building the Examples

### Create Project Structure
```bash
make structure
```

### Build All Examples
```bash
make all        # Build all C and Erlang examples
make all-c      # Build only C examples
make all-erlang # Build only Erlang examples
```

### Build Specific Categories
```bash
# C examples by category
make spatial-c  # Build spatial memory vulnerability examples
make temporal-c # Build temporal memory vulnerability examples

# Erlang examples by category
make spatial-erlang  # Build spatial memory safety examples
make temporal-erlang # Build temporal memory safety examples
```

### Build Individual Examples
```bash
# C examples
make buffer-overflow
make out-of-bounds
make improper-memory
make stack-vulnerability
make use-after-free
make double-free
make memory-leaks
make race-conditions

# Erlang examples
make buffer-safety
make bounds-safety
make memory-safety
make stack-safety
make reference-safety
make garbage-collection
make leak-prevention
make concurrency-safety
```

## Running the Examples

### C Examples

#### Spatial

##### Buffer Overflow
Access the folder:
```bash
cd _build/c-examples/spatial/buffer-overflow
```

The canonical example:
```bash
# Run the example
./example
# show the log of the overflow
sudo dmesg | tail -n 5
```

Pratical example:
```bash
./login
# Exploit
(echo -e "$(./exploit_login.py)"; cat) | ./login
# run the attack
ls
cat password.txt
exit
```


#### Temporal

##### Use After Free
Access the folder:
```bash
cd _build/c-examples/temporal/use-after-free
```

The canonical example:
```bash
# Run the example
./example
```

Pratical example:
```bash
./adduser
# Exploit
./exploit_adduser.py | ./adduser
```

### Erlang Examples

```bash
# Run an Erlang example (from project root)
cd _build/erlang-examples/spatial/buffer-safety
erl
```

## Warning

The C examples in this repository demonstrate real memory vulnerabilities. Running them may cause crashes, undefined behavior, or potential security risks on your system. They are provided for educational purposes only.
