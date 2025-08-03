#!/usr/bin/env bash

set -euo pipefail
cd "$(dirname "$0")"

echo "==> Cleaning..."
make clean

targets=(
  buffer-overflow
  use-after-free
  double-free
  race-conditions
  buffer-safety
  bounds-checking
  reference-safety
  no-manual-free
  concurrency-safety
  out-of-bounds
  debug-buffer-overflow
)

for t in "${targets[@]}"; do
  echo "==> Building: $t"
  make "$t"
done

echo "==> Done."
