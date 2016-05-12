# A comparison of the serialized file sizes of several data structures that map strings to values

The strings used in the comparison are the names of the packages in the [Hackage package index](https://hackage.haskell.org).

The maps are serialized using [`encode` from the `binary` library](http://haddock.stackage.org/lts-5.15/binary-0.7.5.0/Data-Binary.html#v:encode).

## Running the benchmark

```bash
stack build --exec string-map-binary-comparison
```

## Results

```
+----++---------------------------------------+-----------------+-----------------------+---------------+
|    ||                                  Type |         Package | Serialization Library | Size in Bytes |
+====++=======================================+=================+=======================+===============+
|  1 ||                          StringMap () |    TernaryTrees |                binary |        123858 |
|  2 ||                 TernaryMap [Word8] () |    TernaryTrees |                binary |        123858 |
|  3 ||                          StringMap () |  data-stringmap |                binary |        147083 |
|  4 ||                         Map String () |      containers |                binary |        185083 |
|  5 ||                        [(String, ())] |            base |                binary |        185083 |
|  6 ||                         Map String () |      containers |                cereal |        185083 |
|  7 ||                        [(String, ())] |            base |                cereal |        185083 |
|  8 ||                               Trie () | bytestring-trie |                binary |        220212 |
|  9 ||         PatriciaTrieMap AList Char () |      list-tries |                binary |        277307 |
| 10 ||           PatriciaTrieMap Map Char () |      list-tries |                binary |        277307 |
| 11 || PatriciaTrieMap WrappedIntMap Char () |      list-tries |                binary |        277307 |
| 12 ||             ListTrieMap AList Char () |      list-tries |                binary |        570899 |
| 13 ||               ListTrieMap Map Char () |      list-tries |                binary |        570899 |
| 14 ||     ListTrieMap WrappedIntMap Char () |      list-tries |                binary |        570899 |
| 15 ||                       DAWG Char () () |            dawg |                binary |       1166018 |
| 16 ||                      DAWG Word8 () () |            dawg |                binary |       1166018 |
+----++---------------------------------------+-----------------+-----------------------+---------------+
```
