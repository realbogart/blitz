#!/usr/bin/env bash

set -e

echo "Running benchmarks..."
cabal bench
echo "Benchmarks completed!"