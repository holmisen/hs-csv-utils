module Record where

import Data.Array


data Value = Quoted String | Bare String
  deriving (Eq,Ord,Show)


type Record = Array Int Value


makeRecord :: [Value] -> Record
makeRecord = arrayFromList

recordSize = arraySize

maxRecordWidth :: [Record] -> Int
maxRecordWidth = maximum . map recordSize

recordValues :: Record -> [Value]
recordValues = elems

takeFields :: [Int] -> [Record] -> [[Value]]
takeFields fields = map (\r -> arrange r fields)

arrange :: Record -> [Int] -> [Value]
arrange r = foldr (\i xs -> ixDefault (Bare "") r i : xs) []


------------------------------------------------------------
-- Array utils

arrayFromList xs = listArray (1,n) xs where n = length xs

arraySize a = let (l,h) = bounds a in 1 + h - l

ixDefault def a i = if l <= i && i <= h then a!i else def
    where (l,h) = bounds a
