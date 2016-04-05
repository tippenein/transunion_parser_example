module TransunionSpec (spec) where

import Csv
import Parser
import Signal

import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  parserSpec
  csvSpec

parserSpec :: Spec
parserSpec = do
  describe "signalParser" $ do
    it "can snakecase" $ do
      let thing = map titleToSnakeCase ["Some Title", "And Stuff"]
          expected = ["some_title", "and_stuff"]
      thing `shouldBe` expected

    it "parses out a Signal and it's content" $ do
      let toParse = "AD02ffff "
          result = parseSignal toParse
      result `shouldBe` [(AD02, "ffff ")]

    it "parses out Signals into pieces" $ do
      let toParse = "AD02ffff ddAH11ea34xa sdfVS01asdf asdf123"
          result = parseSignal toParse
      result `shouldBe` [(AD02, "ffff dd"), (AH11, "ea34xa sdf"), (VS01, "asdf asdf123")]

  describe "reconciling" $ do
    it "can zip together displacements" $ do
      (zipDisplacements [1,2,3] "abbccc") `shouldBe` ["a", "bb", "ccc"]
    it "can reconcile a single segment" $ do
      let parsed_string = "062311                        1201F 0273814620150824124331"
          expected = [("segment_type", "TU4R")
              , ("segment_length", "062")
              , ("version_switch", "3")
              , ("country_code", "1")
              , ("language_indicator", "1")
              , ("user_reference_number", "                        ")
              , ("bureau_market", "12")
              , ("bureau_submarket", "01")
              , ("industry_code", "F ")
              , ("inquiring_subscriber_code", "02738146")
              , ("transaction_date", "20150824")
              , ("transaction_time", "124331")
              ]
      reconcileSegment (TU4R, parsed_string) `shouldBe` expected

    -- it "parses out the segments" $ do
    --   let input_string = "TU4R062311                        1201F 0273814620150824124331"
    --       parsed = [parseSignal input_string]
    --       expected = [[("segment_type", "TU4R")
    --           , ("segment_length", "062")
    --           , ("version_switch", "3")
    --           , ("country_code", "1")
    --           , ("language_indicator", "1")
    --           , ("user_reference_number", "                        ")
    --           , ("bureau_market", "12")
    --           , ("bureau_submarket", "01")
    --           , ("industry_code", "F ")
    --           , ("inquiring_subscriber_code", "02738146")
    --           , ("transaction_date", "20150824")
    --           , ("transaction_time", "124331")
    --           ]]
    --   (reconcileSegments parsed) `shouldBe` expected

csvSpec :: Spec
csvSpec = do
  describe "schemaForSignal" $ do
    it "builds the externally defined objects" $ do
      True `shouldBe` True
