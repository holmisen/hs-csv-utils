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

module ParseCSV
 ( SValue(..), valueToString, parseSV, parseSVRecord )
where

import Data.Functor.Identity (Identity)
import Data.List             (delete)
import Text.Parsec hiding (newline)

data SValue = Quoted String | Bare String
  deriving (Eq,Ord,Show)


valueToString (Quoted s) = ['"'] ++ s ++ ['"']
valueToString (Bare s)   = s


-- | Parse separated value records. Empty records are removed.
parseSV
  :: SourceName -> Char -> String -> Either ParseError [[SValue]]
parseSV name sep = fmap removeEmptyRecords . runP records sep name


parseSVRecord name sep = runP record sep name


quotedChars = concat `fmap` many qtok where
    qtok = many1 (satisfy (/= '"')) <|> try (string "\"\"")

nonQuotedChars sep = many $ satisfy (`notElem` [sep, '\f', '\n', '\r'])

value =  (Quoted `fmap` try quoted)
     <|> (Bare `fmap` nonQuoted)

quoted = do
  many spaceSV
  char '"'
  x <- quotedChars
  char '"'
  return x

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


removeEmptyRecords = filter (not . emptyRow) where
    emptyRow [Bare []] = True
    emptyRow _         = False

