# A comparison of the serialized file sizes of several data structures that map strings to values

The strings used in the comparison are the names of the packages in the [Hackage package index](https://hackage.haskell.org).

The maps are serialized using [`encode` from the `binary` library](http://haddock.stackage.org/lts-5.15/binary-0.7.5.0/Data-Binary.html#v:encode).

## Running the benchmark

```bash
stack build --exec string-map-binary-comparison
```

## Results

```
+----++-----------------------------------------+-----------------+-----------------------+---------------+
|    ||                                    Type |         Package | Serialization Library | Size in Bytes |
+====++=========================================+=================+=======================+===============+
|  1 ||                            StringMap () |    TernaryTrees |                binary |        123858 |
|  2 ||                   TernaryMap [Word8] () |    TernaryTrees |                binary |        123858 |
|  3 ||                            StringMap () |  data-stringmap |                binary |        147083 |
|  4 ||                           Map String () |      containers |                binary |        185083 |
|  5 ||                          [(String, ())] |            base |                binary |        185083 |
|  6 ||                           Map String () |      containers |                cereal |        185083 |
|  7 ||                          [(String, ())] |            base |                cereal |        185083 |
|  8 ||                                 Trie () | bytestring-trie |                binary |        220212 |
|  9 ||         PatriciaTrieMap AList String () |      list-tries |                binary |        277307 |
| 10 ||           PatriciaTrieMap Map String () |      list-tries |                binary |        277307 |
| 11 || PatriciaTrieMap WrappedIntMap String () |      list-tries |                binary |        277307 |
| 12 ||             ListTrieMap AList String () |      list-tries |                binary |        570899 |
| 13 ||               ListTrieMap Map String () |      list-tries |                binary |        570899 |
| 14 ||     ListTrieMap WrappedIntMap String () |      list-tries |                binary |        570899 |
+----++-----------------------------------------+-----------------+-----------------------+---------------+
```
