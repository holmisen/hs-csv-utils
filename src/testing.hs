{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Parsec


quotedChars = concat `fmap` many qtok where
    qtok = many1 (satisfy (/= '"')) <|> string "\"\""

nonQuotedChars sep = many $ satisfy (`notElem` [sep, '\f', '\n', '\r'])


quote = do
  char '"'
  x <- quotedChars
  char '"'
  return x


data Value = Quoted String | Bare String
 deriving Show

value sep = fmap Quoted (try quote) <|> fmap Bare (nonQuotedChars sep)

values sep = value sep `sepBy` char sep

