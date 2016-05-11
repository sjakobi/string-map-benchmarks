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
import qualified Data.Map as Map
import qualified Data.Map.StringMap as StringMap
import qualified Data.Map.TernaryMap as TernaryMap
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
        [ Result
            "base"
            "[(String, ())]"
            (packedSize mapping)
        , Result
            "TernaryTrees"
            "StringMap ()"
            (packedSize (StringMap.fromList mapping))
        , Result
            "TernaryTrees"
            "TernaryMap [Word8] ()"
            (packedSize (TernaryMap.fromList (map (first ByteString.unpack) bsMapping)))
        , Result
            "containers"
            "Map String ()"
            (packedSize (Map.fromList mapping))
        , Result
            "bytestring-trie"
            "Trie ()"
            (packedSize (BSTrie.fromList bsMapping))
        ]

      table = tabulateResults results

  outputTable table

outputTable :: Table String String String -> IO ()
outputTable = putStrLn . AsciiArt.render id id id

data Result a = Result
  { resultPackage :: String
  , resultType :: String
  , resultContent :: a
  } deriving (Show, Eq, Ord, Functor)

packedSize :: Binary.Binary a => a -> Int64
packedSize = LazyByteString.length . Binary.encode

tabulateResults :: Show a => [Result a] -> Table String String String
tabulateResults results =
  Table
    (Group NoLine (map (Header . resultType) results))
    (Group SingleLine [Header "Package", Header "Size in Bytes"])
    (map (\r -> [resultPackage r, show (resultContent r)]) results)
