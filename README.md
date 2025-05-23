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

### Per language

| Vulnerability | C | Pascal | Java | Erlang |
|---------------|---|--------|------|--------|
| Buffer overflow | ✓ | ▲ | ✗* | ✗ |
| Use after free | ✓ | ✓ | ✗* | ✗ |
| Buffer underflow | ✓ | ▲ | ✗* | ✗ |
| Double free | ✓ | ✓ | ✗* | ✗ |
| Array index out-of-bounds | ✓ | ▲ | ✗* | ✗ |
| Dangling pointers | ✓ | ✓ | ✗* | ✗ |
| Pointer arithmetic errors | ✓ | ✗** | ✗* | ✗ |
| Memory leaks | ✓ | ✓ | ▲** | ▲** |
| Uninitialized memory reads | ✓ | ✓ | ✗* | ✗ |
| Data race conditions | ✓ | ✓ | ✓ | ✗*** |

#### Legend:
- ✓: Vulnerability exists in this language
- ✗: Vulnerability does not exist or is prevented by language design
- ▲: Partially mitigated through language design but still possible
- ✗*: Prevented through automatic memory management (garbage collection)
- ▲**: Can still have "logical" memory leaks despite garbage collection
- ✗**: Standard Pascal doesn't allow pointer arithmetic operations like C
- ✗***: Prevented through Erlang's unique message-passing concurrency model

#### Language-Specific Memory Management Notes:

**C:**
- Manual memory management with malloc/free
- No automatic bounds checking
- Direct pointer manipulation and arithmetic
- Programmer responsible for all memory safety

**Pascal:**
- Manual memory management with new/dispose
- Optional array bounds checking in many implementations
- Strongly typed pointers with type-safety checks
- No arbitrary pointer arithmetic like in C
- Still vulnerable to dangling pointers and use-after-free issues

**Java:**
- Automatic memory management via garbage collection
- Automatic array bounds checking
- No direct pointer manipulation (uses references)
- Still vulnerable to logical memory leaks (holding onto references unnecessarily)
- Shared-memory concurrency model allows for race conditions

**Erlang:**
- Automatic memory management via garbage collection
- Immutable data structures prevent many vulnerabilities
- No pointers or direct memory manipulation
- Message-passing concurrency model prevents traditional race conditions
- "Share nothing" architecture between processes

## Building the Examples

### Compile all its already done
```bash
sh compile.sh
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
make use-after-free
make memory-leaks

# Erlang examples
make buffer-safety
make reference-safety
```

## Running the Examples

### Vulnerability Examples

#### Buffer Overflow

<details>
<summary>How to run Buffer Overflow examples</summary>

```bash
cd _build/vulnerability-examples/spatial/buffer-overflow
```

Run the canonical example:

```bash
./example
sudo dmesg | tail -n 5
```

Run the practical exploit:

```bash
./login
(echo -e "$(./exploit_login.py)"; cat) | ./login
ls
cat password.txt
exit
```

</details>

#### Out-of-Bounds

<details>
<summary>How to run Out-of-Bounds examples</summary>

```bash
cd _build/vulnerability-examples/spatial/out-of-bounds
```

Run the canonical example:

```bash
./example
```

Run the practical exploit:

```bash
./cal
echo "1984" | exploit_cal.py
```

</details>

#### Use-After-Free

<details>
<summary>How to run Use-After-Free examples</summary>

```bash
cd _build/vulnerability-examples/temporal/use-after-free
```

Run the canonical example:

```bash
./example
```

Run the practical exploit:

```bash
./useradd
./exploit_useradd.py | ./useradd
```

</details>

#### Race Conditions

<details>
<summary>How to run Race Conditions examples</summary>

```bash
cd _build/vulnerability-examples/temporal/race-conditions
```

Run the canonical example:

```bash
java Example
```

Run the practical exploit:

```bash
java BookScraper "shakespeare"
```
> This will "work" but the downloaded EPUB files will be corrupted because race condition on write.

</details>

---

### Erlang Examples

> All Erlang examples can be run using the Erlang shell (`erl`) after compiling.

#### Buffer Safety

<details>
<summary>How to run Buffer Safety examples</summary>

1. Navigate to the compiled output folder. For example:

   ```bash
   cd _build/erlang-examples/spatial/buffer-safety
   ```

2. Open the Erlang shell:

   ```bash
   erl
   ```

3. Run the module:

   ```erlang
   example:handle_string("This is a very long string that would break a fixed size buffer in C").
   login:validate_password("wrong_password").
   login:validate_password("tryBreak_this1").
   ```
</details>

#### Reference Safety

<details>
<summary>How to run Reference Safety examples</summary>

1. Navigate to the compiled output folder. For example:

   ```bash
   cd _build/erlang-examples/temporal/reference-safety
   ```

2. Open the Erlang shell:

   ```bash
   erl
   ```

3. Run the module:

   ```erlang
   example:safe_memory().
   useradd:init().
     > login admin admin123
     > adduser camilo pass 0
     > logout
     > adduser adolfo pass 1
     > login camilo pass
     > adduser adolfo pass 1 
     > logout
     > exit
   ```
</details>

## Future Goals
- [ ] Dockerize so don't have to run this on local machine
- [ ] Write Unit Tests with Criterion for C Code to demonstrate it passes even its broke

## Warning

The C examples in this repository demonstrate real memory vulnerabilities. Running them may cause crashes, undefined behavior, or potential security risks on your system. They are provided for educational purposes only.
