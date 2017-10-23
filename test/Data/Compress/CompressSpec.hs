{-# LANGUAGE OverloadedStrings #-}
module Data.Compress.CompressSpec where


import Test.Hspec
import Test.QuickCheck

import Data.Compress.Compress

spec :: Spec
spec =
  describe "Compress" $
    describe "for any string value" $ do
      it "can compress" $
        compress "abcd" `shouldBe` "x\156KLJN\SOH\NUL\ETX\216\SOH\139"
      it "for any string value" $
        decompress "x\156KLJN\SOH\NUL\ETX\216\SOH\139" `shouldBe` "abcd"