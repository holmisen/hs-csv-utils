{-# LANGUAGE NoMonomorphismRestriction #-}

-- | A CSV file parser. This can in fact parse delimeter separated
-- values with "any" delimeter.
--
--  * Can handle any delimeter
--
--  * Can handle double quoted values
--
--  * Can handle different line endings
--
--  * Can NOT handle single quoted values
--
--  * Can NOT handle double double quotes
--

module ParseCSV
 ( parseSV, parseSVRecord )
where

import Data.Functor.Identity (Identity)
import Data.List             (delete)
import Text.Parsec hiding (newline)


parseSV name sep =
    fmap removeEmptyRecords . runP records sep name


parseSVRecord name sep = runP record sep name


value = do
  s1 <- many spaceSV
  xs <- try quoted <|> nonQuoted
  s2 <- many spaceSV
  return (s1 ++ xs ++ s2)

quoted = do
  char '"'
  xs <- many $ satisfy (`notElem` ['"'])
  char '"'
  return (['"'] ++ xs ++ ['"'])

nonQuoted = do
  sep <- getState
  many $ satisfy (`notElem` [sep, '\n', '\r', '\f'])

record = do
  sep <- getState
  value `sepBy` char sep

records = record `sepEndBy` newline


-- Use special newline parser to cope with different line endings.
newline = many1 $ choice $ map string ["\n", "\f", "\r"]

-- Use special space parser to cope with different record separators.
spaceSV = do
  sep <- getState
  choice $ map char $ delete sep spaceChars
  where
    spaceChars = [' ', '\t', '\v']


removeEmptyRecords = filter (/= [""])
