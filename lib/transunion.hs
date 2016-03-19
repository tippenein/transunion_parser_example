{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Transunion where

-- import Control.Applicative
import Control.Applicative (empty)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Text (Text)
import Safe (readMay)
import Text.Parsec

import Signal

toSignal :: String -> Signal
toSignal s = read s :: Signal

type Parser = Parsec String ()

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

parseSignal :: String -> SignalMap
parseSignal input = Map.fromList result
  where
    result = case parse' (many anySignal) input of
               Left e -> error "woops"
               Right r -> r

anySignal :: Parser (Signal, String)
anySignal = do
  signal <- signalParser
  content <- manyTill anyToken (lookAhead signalParser)
  return (toSignal signal, content)

signalParser :: Parser String
signalParser = choice $ string <$> allSignals

