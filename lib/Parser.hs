{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser where

-- import Control.Applicative
import Control.Applicative (empty)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Text (Text)
import Safe (readMay)
import Text.Parsec

import Csv
import Signal

$(declareSignal Csv.allSignals)

toSignal :: String -> Signal
toSignal s = read s :: Signal

type Parser = Parsec String ()

type SignalMap = Map.Map Signal String
type SignalConst = (Signal, String)

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

eol = char '\n' *> return ()

endOfLineOrInput :: Parser ()
endOfLineOrInput = eol <|> eof

parseSignal :: String -> SignalMap
parseSignal input = Map.fromList result
  where
    result = case parse' (many anySignal) input of
               Left err -> error (show err)
               Right r -> r

anySignal :: Parser (Signal, String)
anySignal = do
  signal <- signalParser
  content <- manyTill anyToken (endOfLineOrInput <|> signalLookahead)
  return (toSignal signal, content)

signalLookahead = lookAhead signalParser *> return ()

signalParser :: Parser String
signalParser = choice $ fmap try $ string <$> allSignals

type TUJson = (String, String)

fullReconcile :: SignalMap -> [TUJson]
fullReconcile sm = undefined -- Map.fold fn [] sm

