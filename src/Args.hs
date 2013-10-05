module Args 
 ( Args
 , getArgs
 , parseArgs
 , lookup
 , lookupDefault
 , rest
 )
where

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified System.Environment as Sys


type Argm = Map String String

data Args = Args { named :: Argm, unnamed :: [String] }
 deriving Show

mapNamed f args = args { named = f $ named args }

parseArgs :: [String] -> Args
parseArgs = go Map.empty where
    go m (p@('-':_):as) =
        case as of
          (('-':_):_) -> go (Map.insert p [] m) as
          (v:as)      -> go (Map.insert p v m) as
          as          -> go (Map.insert p [] m) as
    go m as = Args m as


lookup :: String -> Args -> Maybe String
lookup name = Map.lookup name . named

lookupDefault :: String -> String -> Args -> String
lookupDefault name def = maybe def id . lookup name

rest :: Args -> [String]
rest = unnamed

getArgs :: IO Args
getArgs = parseArgs `fmap` Sys.getArgs
