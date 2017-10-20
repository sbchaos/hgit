module Main where

import Data.Storage.Parser

main :: IO ()
main = do
  path1 <- readFile ".git/HEAD"
  print path1
  let (_: path2) = dropWhile (/=' ') path1
  let (x : path) = reverse path2
  print ("the path here " ++ path)
  file <- readFile (".git/" ++ reverse path)
--  print gitObject content
  print file
