{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Csv where

import qualified Data.ByteString.Lazy as LB
import Data.Char
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub)
import Data.Csv as Csv
import qualified Data.Vector as V
import GHC.Generics

data TUData
  = TUData {
    _tuFFRCode    :: !String  --- should be Signal
  , _fieldName    :: !String
  , _displacement :: !Integer
  , _length       :: !Integer
  , _type         :: !String
  } deriving (Generic, Show, Eq)

instance Ord TUData where
  (TUData _ _ d1 _ _) `compare` (TUData _ _ d2 _ _) = d1 `compare` d2

instance FromRecord TUData where

readCsv :: IO [TUData]
readCsv = do
  csvData <- LB.readFile "TU41_full_csv_data_file.csv"
  let datas = Csv.decode HasHeader csvData :: Either String (V.Vector TUData)
    in case datas of
      Left _ -> fail "bad csv format"
      Right m -> return $ V.toList m

schemaForSignal :: String -> IO [TUData]
schemaForSignal signal = filter ((==) signal . _tuFFRCode) <$> readCsv

{-# NOINLINE allSignals #-}
allSignals :: [String]
allSignals = unsafePerformIO g
  where
    g = nub . map _tuFFRCode <$> readCsv
