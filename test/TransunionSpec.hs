module TransunionSpec (spec) where

import Signal
import Transunion

import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec =
  describe "signalParser" $ do
    it "parses out a Signal" $ do
      let toParse = "AD02ffff "
          result = parseSignal toParse
      result `shouldBe` Map.fromList [(AD02, "ffff ")]
    it "parses out Signals into pieces" $ do
      let toParse = "AD02ffff ddAH11ea34xa sdfVS01asdf asdf123"
          result = parseSignal toParse
      result `shouldBe` Map.fromList [(AD02, "ffff dd"), (AH11, "ea34xa sdf"), (VS01, "asdf asdf123")]
