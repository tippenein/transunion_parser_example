module TransunionSpec (spec) where

import Transunion

import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec =
  describe "signalParser" $ do
    it "parses out Signals into pieces" $ do
      let toParse = "AD02ffff ddAH11ea34xa sdfVS01asdf asdf123"
          result = parse' parseSignal toParse
      result `shouldReturn` Map.fromList [(AD02, "ffff dd"), (AH11, "ea34xa sdf"), (VS01, "asdf asdf123")]
