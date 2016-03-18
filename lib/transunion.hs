{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Transunion where

-- import Control.Applicative
import qualified Data.Map as Map
import Data.Text (Text)
import Safe (readMay)
import Text.Parsec

import Signal

readMaybe = readMay

type Parser = Parsec String ()

parseSignal :: String -> SignalMap
parseSignal input = Map.fromList $ parse' (many anySignal) input

anySignal :: Parser (Signal, String)
anySignal = parseWithLeftOver signalParser

-- try mconcat over allSignals
signalParser :: Parser Signal
signalParser = string "AD02" <|> string "VS01" <|> string "AH11"

fromSignal :: String -> Maybe Signal
fromSignal s = readMaybe s :: Maybe Signal

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse' ((,) <$> return (fromSignal p) <*> leftOver)
  where leftOver = manyTill anyToken eof

parse' rule text = parse rule "(source_file)" text


