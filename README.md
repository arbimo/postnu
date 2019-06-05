This project provides the implementation of algorithms for reasoning on POSTNU (Partially Observable Simple Temporal Networks with Uncertainty)

# Install

If you haven't done so yet, [install the rust toolchain](https://www.rust-lang.org/tools/install).

You can now use `cargo` to compile and run from the sources:

```bash
cargo run -- [options]
```


# Task Networks

This repository provides a few example of POSTNU that are stored in `networks/sat` for the ones that are dynamically controllable (DC) and `networks/unsat` for those that are not.

For instance, running the tool on a first DC instance can be done with the following command (the `-v` option is used to enable a verbose output): 

```bash
cargo run -- -v networks/sat/1.tn 
``` 


This file format is as folllow:

 - a line starting with `#` is a comment and is ignored
 - a line `A B obs 10 20` indicates an observable contingent link whose duration will be within `[10,20]` time units. This implies that B is a contingent timepoint whose occurrence time can be observed. 
 - a line `A B hid 10 20` indicates a hidden contingent link  whose duration will be within `[10,20]` time units. This implies that B is a contingent timepoint whose occurrence time can NOT be observed.
 - a line `A B req 10 20` is a requirement link from A to B with label `[10,20]`. It correspond to the constraint $10 \leq B - A \leq 20$. Note that the lower or upper bound can be omitted by replacing it with an underscore `_`.   
