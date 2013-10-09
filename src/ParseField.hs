{-# LANGUAGE NoMonomorphismRestriction #-}

module ParseField 
 ( Field(..)
 , enumFields
 , fields
 , field
 , parse
 ) 
where

import Control.Applicative ((<*))
import Text.Parsec


data Field = One Int
           | IntervalFrom Int
           | Interval Int Int
 deriving (Eq,Show)


enumFields :: Int -> [Field] -> [Int]
enumFields n = go where
    go (One i          : fs) = i : go fs
    go (IntervalFrom i : fs) = [i..n] ++ go fs
    go (Interval i j   : fs) = [i..j] ++ go fs
    go []                    = []


fields = field `sepBy` symbol ","

field = choice [try interval, try intervalFrom, one]

positive = read `fmap` lexeme (many1 digit)

one = One `fmap` positive

intervalFrom = do
  p <- positive
  symbol "-"
  return (IntervalFrom p)

interval = do
  i <- positive
  symbol "-"
  j <- positive
  return (Interval i j)


lexeme p = p <* spaces

symbol = lexeme . string
