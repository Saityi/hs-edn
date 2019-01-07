module Main where

import Api
import Data.Text
import Data.Foldable

main :: IO ()
main =
  (readFile "test.edn") >>=
  mapM_ (mapM_ print) . parseEdnElements . pack
