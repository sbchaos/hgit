module Data.Storage.ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec
import Data.Storage.Parser
import Data.Storage.ObjectTypes

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
--main :: IO ()
--main = hspec spec

spec :: Spec
spec =
  describe "Parser" $ do
    describe "git type" $ do
      it "parses the tree to proper object" $
        parse gitType "test" "tree" `shouldBe` Right GTree
      it "parses the blob to proper object" $
        parse gitType "test" "blob" `shouldBe` Right GBlob
    describe "header" $
      it "parser header to tuple value" $
        parse header "test" "tree 12\0" `shouldBe` Right (GTree, 12)
    describe "reads blob content" $
      it "parses content" $
        parse (blob 5) "test" "abcdefgh" `shouldBe` Right "abcde"
    describe "reads tree enty" $
      it "parses entry successfully" $
        parse entry "test" "100644 blob 7cf3dfaffc0a7335330a7c35676dbcfdc4af5466  README.md\0" `shouldBe` Right  TreeEntry { mode="100644", gtype=GBlob, hash="7cf3dfaffc0a7335330a7c35676dbcfdc4af5466", path="README.md"}
