# A Haskell implementation of HoneyBadgerBFT

The research and protocols for this algorithm are explained in detail in [The Honey Badger of BFT Protocols by Miller et al., 2016](https://eprint.iacr.org/2016/199.pdf).

The implementation is WIP.

## Install and Run

Get [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

Install [Rust](https://www.rust-lang.org/en-US/install.html).

Install [LLVM 3.7](http://releases.llvm.org/download.html).

Clone the repo, enter the folder, and build the project:

```
stack setup
./build_dylibs
./build
```

`build_dylibs` builds shared libraries required for FFI. Through FFI, we call the [threshold_crypto](https://github.com/poanetwork/threshold_crypto) Rust library, which provides threshold encryption functionality.

Run the executable:

```
stack exec HoneyBadgerBFT-exe
```

Run tests:

```
./run_tests
```

## Limitations

- The maximum number of shards supported by the erasure coding scheme library we use is 257. We need to find a way around it to accommodate networks with a much bigger number of validators.
