# A comparison of the serialized file sizes of several data structures that map strings to values

The strings used in the comparison are the names of the packages in the [Hackage package index](https://hackage.haskell.org).

The maps are serialized using [`encode` from the `binary` library](http://haddock.stackage.org/lts-5.15/binary-0.7.5.0/Data-Binary.html#v:encode).

## Runing the benchmark

```haskell
stack build --exec string-map-binary-comparison
```
