{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Lib (openApi)

main :: IO ()
main = do
  LBS.putStr $ encodePretty openApi
  putStrLn ""
