import qualified Args
import ParseSV
import ParseField as F
import Record
import Utils

import Control.Monad
import Data.Array
import Data.List   (intercalate)
import Data.Maybe
import System.IO
import System.Exit


main = do
  args <- Args.getArgs
  let infile  = listToMaybe $ Args.rest args
  let sep     = fmap head (Args.lookup "-d" args)
               <|> join (fmap sepFromFileExtension infile)
               <|> defaultSep
  let outsep  = head $ Args.lookupDefault "-s" [sep] args
  let fs      = Args.lookupDefault "-f" [] args
  let srcName = maybe "<stdin>" id $ infile
  input <- maybe getContents readFile $ infile

  let res = do
         records <- parseRecords srcName sep input
         let n = maxRecordWidth records
         fields <- F.enumFields n `fmap` F.parse F.fields "arguments" fs
         return (records, fields)
  
  case res of
    Left er -> do
         hPutStrLn stderr (show er)
         exitFailure
    Right (records, fields) ->
        mapM_ putStrLn $ map (unparseValues outsep) $ takeFields fields records
         
