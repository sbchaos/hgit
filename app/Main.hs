{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Storage.Parser
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Compress.Compress

main :: IO ()
main = do
  path1 <- readFile ".git/HEAD"
  print path1
  let (_:path2) = dropWhile (/= ' ') path1
  let (x:path) = reverse path2
  print ("the path here " ++ path)
  file <- readFile (".git/" ++ reverse path)
  commit <- B.readFile ".git/objects/46/32c6921c87209d5f60b6e498be0dc1ea2a80f7"
  let str = C.unpack $ decompress commit
  let parsed = parseGitObject str
  print parsed
