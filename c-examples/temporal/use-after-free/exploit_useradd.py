#!/usr/bin/env python3

# Simulate memory manipulation
for _ in range(5):
    print("login admin admin123")  # Attempt to reallocate same memory
    print("logout")  # Free again

print("adduser hacker password 1")  # Attempt to reallocate memory with controlled content
print("login hacker password")  # Attempt to reallocate memory with controlled content

# Attempt to trigger unexpected behavior
print("admin")
print("exit")
