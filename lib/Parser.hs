{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parser where

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
  signal <- signalParser
  content <- manyTill anyToken (endOfLineOrInput <|> signalLookahead)
  return (toSignal signal, content)

signalLookahead = lookAhead signalParser *> return ()

signalParser :: Parser String
signalParser = choice $ fmap try $ string <$> allSignals

parseSignal :: String -> SignalMap
parseSignal input =
  case parse' (many anySignal) input of
    Left err -> error (show err)
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
cutSegments (n:rest) content =
  take n content : cutSegments rest (drop n content)

schemaForSignal :: Signal -> [TUData]
schemaForSignal signal =
  drop 1 $ sort $ -- ignore type info, we already know it
    unsafePerformIO (filter ((==) (show signal) . _tuFFRCode) <$> readCsv)

