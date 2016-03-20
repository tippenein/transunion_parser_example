{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

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

eol = char '\n'

endOfLineOrInput :: Parser ()
endOfLineOrInput = eol *> return () <|> eof

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

