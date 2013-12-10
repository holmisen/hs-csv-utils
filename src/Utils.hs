module Utils where

import System.FilePath (takeExtension)


defaultSep = '\t'

sepFromFileExtension :: FilePath -> Maybe Char
sepFromFileExtension = sep . takeExtension where
  sep ".csv" = Just ','
  sep ".tsv" = Just '\t'
  sep ".psv" = Just '|'
  sep _      = Nothing


Nothing <|> x = x
Just x  <|> _ = x

infixr 9 <|>