# A comparison of the serialized file sizes of several data structures that map strings to values

The strings used in the comparison are the names of the packages in the [Hackage package index](https://hackage.haskell.org).

The maps are serialized using [`encode` from the `binary` library](http://haddock.stackage.org/lts-5.15/binary-0.7.5.0/Data-Binary.html#v:encode).

## Running the benchmark

```bash
stack build --exec string-map-binary-comparison
```

## Results

```
+----++-----------------------------------------+-----------------+---------------+
|    ||                                    Type |         Package | Size in Bytes |
+====++=========================================+=================+===============+
|  1 ||                            StringMap () |    TernaryTrees |        123858 |
|  2 ||                   TernaryMap [Word8] () |    TernaryTrees |        123858 |
|  3 ||                            StringMap () |  data-stringmap |        147083 |
|  4 ||                           Map String () |      containers |        185083 |
|  5 ||                          [(String, ())] |            base |        185083 |
|  6 ||                                 Trie () | bytestring-trie |        220212 |
|  7 ||         PatriciaTrieMap AList String () |      list-tries |        277307 |
|  8 ||           PatriciaTrieMap Map String () |      list-tries |        277307 |
|  9 || PatriciaTrieMap WrappedIntMap String () |      list-tries |        277307 |
| 10 ||             ListTrieMap AList String () |      list-tries |        570899 |
| 11 ||               ListTrieMap Map String () |      list-tries |        570899 |
| 12 ||     ListTrieMap WrappedIntMap String () |      list-tries |        570899 |
+----++-----------------------------------------+-----------------+---------------+
```
