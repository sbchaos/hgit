module Data.GitPath.GitPath where

import System.FilePath ((</>))

refPath :: FilePath -> String -> FilePath
refPath gitDir ref = let
   (dir,file) = splitAt 2 ref
   in gitDir </> "objects" </> dir </> file
