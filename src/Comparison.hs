{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Comparison
  ( run
  ) where

import           Control.Arrow
import qualified Data.Binary as Binary
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as BSChar8
import           Data.Int
import           Data.List
import           Data.ListTrie.Base.Map (AList, WrappedIntMap)
import qualified Data.ListTrie.Map as ListTrie
import qualified Data.ListTrie.Patricia.Map as Patricia
import qualified Data.Map as Map
import qualified Data.Map.StringMap as TernaryStringMap
import qualified Data.Map.TernaryMap as TernaryMap
import qualified Data.Serialize as Serialize
import qualified Data.StringMap.Strict as StringMap
import qualified Data.Trie as BSTrie
import qualified System.IO.Strict as StrictIO
import           Text.Tabular (Table(..), Properties(..), Header(..))
import qualified Text.Tabular.AsciiArt as AsciiArt

run :: IO ()
run = do
  pkgs <- lines <$> StrictIO.readFile "packages-on-hackage.txt"
  let mapping = map (, ()) pkgs
      bsMapping = map (first BSChar8.pack) mapping

      results =
        [ -- binary
          Result
            "base"
            "[(String, ())]"
            "binary"
            (encodedSize mapping)
        , Result
            "TernaryTrees"
            "StringMap ()"
            "binary"
            (encodedSize (TernaryStringMap.fromList mapping))
        , Result
            "TernaryTrees"
            "TernaryMap [Word8] ()"
            "binary"
            (encodedSize (TernaryMap.fromList (map (first ByteString.unpack) bsMapping)))
        , Result
            "containers"
            "Map String ()"
            "binary"
            (encodedSize (Map.fromList mapping))
        , Result
            "bytestring-trie"
            "Trie ()"
            "binary"
            (encodedSize (BSTrie.fromList bsMapping))
        , Result
            "data-stringmap"
            "StringMap ()"
            "binary"
            (encodedSize (StringMap.fromList mapping))
        , Result
            "list-tries"
            "ListTrieMap WrappedIntMap String ()"
            "binary"
            (encodedSize
              (ListTrie.fromList mapping :: ListTrie.TrieMap WrappedIntMap Char ()))
        , Result
            "list-tries"
            "ListTrieMap AList String ()"
            "binary"
            (encodedSize
              (ListTrie.fromList mapping :: ListTrie.TrieMap AList Char ()))
        , Result
            "list-tries"
            "ListTrieMap Map String ()"
            "binary"
            (encodedSize
              (ListTrie.fromList mapping :: ListTrie.TrieMap Map.Map Char ()))
        , Result
            "list-tries"
            "PatriciaTrieMap WrappedIntMap String ()"
            "binary"
            (encodedSize (Patricia.fromList mapping :: Patricia.TrieMap WrappedIntMap Char ()))
        , Result
            "list-tries"
            "PatriciaTrieMap AList String ()"
            "binary"
            (encodedSize (Patricia.fromList mapping :: Patricia.TrieMap AList Char ()))
        , Result
            "list-tries"
            "PatriciaTrieMap Map String ()"
            "binary"
            (encodedSize (Patricia.fromList mapping :: Patricia.TrieMap Map.Map Char ()))

          -- cereal
        , Result
            "base"
            "[(String, ())]"
            "cereal"
            (serializedSize mapping)
        , Result
            "containers"
            "Map String ()"
            "cereal"
            (serializedSize (Map.fromList mapping))
        ]
      table = tabulateResults (sortOn (\(Result p t l c) -> (c, l, t, p)) results)

  outputTable table

outputTable :: Table String String String -> IO ()
outputTable = putStrLn . AsciiArt.render id id id

data Result a = Result
  { resultPackage :: String
  , resultType :: String
  , resultSerializationLibrary :: String
  , resultContent :: a
  } deriving (Show, Eq, Ord, Functor)

encodedSize :: Binary.Binary a => a -> Int64
encodedSize = LazyByteString.length . Binary.encode

serializedSize :: Serialize.Serialize a => a -> Int64
serializedSize = LazyByteString.length . Serialize.encodeLazy

tabulateResults :: Show a => [Result a] -> Table String String String
tabulateResults results =
  Table
    (Group NoLine (map (Header . show) [1 .. length results]))
    (Group
      SingleLine
      (map
        Header
        ["Type", "Package", "Serialization Library", "Size in Bytes"]))
    (map (\(Result p t l c) -> [t, p, l, show c]) results)
