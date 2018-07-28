# A Haskell implementation of HoneyBadgerBFT

The research and protocols for this algorithm are explained in detail in [The Honey Badger of BFT Protocols by Miller et al., 2016](https://eprint.iacr.org/2016/199.pdf).

The implementation is WIP.

## Install and Run

Get [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

Clone the repo, enter the folder, and build the project:

```
stack setup
stack build
```

Run the executable:

```
stack exec HoneyBadgerBFT-exe
```


## Limitations

- The maximum number of shards supported by the erasure coding scheme library we use is 257. We need to find a way around it to accommodate networks with a much bigger number of validators.
