#!/bin/bash

set -e

cargo build --release
EXE="./target/release/postnu"

for filename in networks/sat/*.tn; do
    echo "Running SAT: $filename"
    $EXE $filename --controllable true >> /dev/null
done


for filename in networks/unsat/*.tn; do
    echo "Running UNSAT: $filename"
    $EXE $filename --controllable false >> /dev/null
done
