module Main (main) where

import System.Environment
import qualified Transunion

main :: IO ()
main = do
  res <- Transunion.parse <$> getArgs
  case res of
    Left _ -> error "invalid input"
    Right a -> a

