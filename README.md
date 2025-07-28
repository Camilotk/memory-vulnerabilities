# Memory Vulnerabilities: C and Erlang Comparison

This repository contains code examples from the future research paper "Erlang's Unsafe Impedance: A Design-Level Prevention of Common Memory Vulnerabilities". It demonstrates common memory vulnerabilities in C and how they are prevented by design in Erlang.

## Repository Structure

The repository is organized by vulnerability type, with matching implementations in C (vulnerable) and Erlang (safe):

```
memory-vulnerabilities/
├── vulnerability-examples/   # Vulnerable implementations in C, Java, Pascal
│   ├── spatial/
│   └── temporal/
└── safety-examples/          # Safe implementations in Erlang
    ├── spatial/
    └── temporal/
```

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
make double-free

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

#### Double Free

<details>
<summary>How to run Double Free examples</summary>

```bash
cd _build/vulnerability-examples/temporal/double-free
```

Run the canonical example:

```bash
./example
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
   cd _build/safety-examples/spatial/buffer-safety
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
   cd _build/safety-examples/temporal/reference-safety
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

## Warning

The C examples in this repository demonstrate real memory vulnerabilities. Running them may cause crashes, undefined behavior, or potential security risks on your system. They are provided for educational purposes only.
