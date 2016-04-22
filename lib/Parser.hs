{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parser where

import Data.List (sort)
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec

import Csv
import Signal
import Util

$(declareSignal Csv.allSignals)

toSignal :: String -> Signal
toSignal s = read s :: Signal

type Parser = Parsec String ()
type SignalMap = [(Signal, String)]
type SignalSegment = (String, String)

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

eol = char '\n' *> return ()

endOfLineOrInput :: Parser ()
endOfLineOrInput = eol <|> eof

anySignal :: Parser (Signal, String)
anySignal = do
  signal <- signalParser <?> "a Signal somewhere"
  let total = sum $ map _length $ schemaForSignal $ toSignal signal

  content <- count total anyToken <?> show total <> " characters in segment for " <> signal
  return (toSignal signal, content)

signalLookahead = lookAhead signalParser *> return ()

signalParser :: Parser String
signalParser = choice (fmap try $ string <$> allSignals) <?> "Signal string"

parseSignal :: String -> SignalMap
parseSignal input =
  case parse' (many1 anySignal) input of
    Left e -> error $ show e
    Right r -> r

reconcileSegments :: String -> [(Signal, [SignalSegment])]
reconcileSegments = map reconcileSegment . parseSignal

reconcileSegment :: (Signal, String) -> (Signal, [SignalSegment])
reconcileSegment seg =
  let (signal,content) = seg
      fields = map (titleToSnakeCase . _fieldName) schema
      schema = schemaForSignal signal
      displacements = map _length schema
      contents = cutSegments displacements content
   in
     (signal, zip fields contents)

cutSegments :: [Int] -> String -> [String]
cutSegments [] _ = []
cutSegments ns "" = error "ran out of string to parse"
cutSegments (n:rest) content =
  take n content : cutSegments rest (drop n content)

schemaForSignal :: Signal -> [TUData]
schemaForSignal signal =
  drop 1 $ sort $ -- ignore type info, we already know it
    unsafePerformIO (filter ((==) (show signal) . _tuFFRCode) <$> readCsv)

