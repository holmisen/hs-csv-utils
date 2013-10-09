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
--  * Fault tolerant -- can handle bad quoting
--

module ParseSV
 ( Value(..)
 , Record(..)
 , parseRecords
 , unparseValue
 , unparseValues
 , unparseRecord
 , unparseRecords
 )
where

import Record

import Data.Functor.Identity (Identity)
import Data.List             (delete, intercalate)
import Text.Parsec hiding    (newline)


unparseValue (Quoted s) = ['"'] ++ concatMap q s ++ ['"']
    where q '"' = "\"\""
          q c   = [c]
unparseValue (Bare s)   = s

unparseValues :: Char -> [Value] -> String
unparseValues sep = intercalate [sep] . map unparseValue

unparseRecord :: Char -> Record -> String
unparseRecord sep = unparseValues sep . recordValues

unparseRecords :: Char -> [Record] -> String
unparseRecords sep = unlines . map (unparseRecord sep)


parseRecords
  :: SourceName -> Char -> String -> Either ParseError [Record]
parseRecords name sep = fmap (map makeRecord . removeEmptyRecords) . runP records sep name


parseRecord name sep = runP record sep name


quotedChars = many qchar where
    qchar = satisfy (/= '"') <|> try (string "\"\"" >> return '"')

nonQuotedChars sep = many $ satisfy (`notElem` [sep, '\f', '\n', '\r'])

value =  (Quoted `fmap` try quoted)
     <|> (Bare `fmap` nonQuoted)

quoted = many spaceSV >> between (char '"') (char '"') quotedChars

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

