{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parser where

import Control.Applicative (empty)
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Text (Text)
import Safe (readMay)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec

import Csv
import Signal

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

parseSignal :: String -> SignalMap
parseSignal input =
  case parse' (many anySignal) input of
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

schemaForSignal :: String -> [TUData]
schemaForSignal signal = unsafePerformIO g
  where
    g = filter ((==) signal . _tuFFRCode) <$> readCsv

reconcileSegments :: String -> [[SignalSegment]]
reconcileSegments stringInput =
  let segments = parseSignal stringInput
   in
    map reconcileSegment segments

reconcileSegment :: (Signal, String) -> [SignalSegment]
reconcileSegment seg =
  let displacements = map _length schema
      fields = map titleToSnakeCase $ map _fieldName schema
      signal = fst seg
      content = show signal ++ snd seg
      schema = sort $ schemaForSignal (show signal)
      contents = zipDisplacements displacements content
   in
     zipWith (,) fields contents

zipDisplacements :: [Int] -> String -> [String]
zipDisplacements [] _ = []
zipDisplacements (i:rest) content =
  take i content : zipDisplacements rest (drop i content)

titleToSnakeCase :: String -> String
titleToSnakeCase s = concatMap underscore s

underscore :: Char -> String
underscore ' ' = "_"
underscore c = [toLower c]
