import qualified Args
import ParseCSV

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Maybe
import Data.List       (isPrefixOf)
import Data.List.Split (splitOn)
import Graphics.UI.Gtk
import System.Exit (exitFailure)


defaultSep = "\t"

main = do

  -- Get args etc
  args <- Args.getArgs
  let sep = head $ Args.lookupDefault "-d" defaultSep args
  let header = isJust $ Args.lookup "-h" args
  let infile = listToMaybe $ Args.rest args
  
  -- Read data
  input <- maybe getContents readFile infile

  -- Parse data
  records <- parseDataOrExit [] sep input
  let ldata :: [Array Int SValue]
      ldata  = map mkRow $ if header then tail records else records
  let cols   = maximum $ map arraySize ldata
  let cnames :: Array Int String
      cnames = if header then mkRow (map valueToString $ head records) else listArray (1,cols) $ map show [1..cols]

  -- Start GUI
  initGUI
  win <- windowNew
  win `set` [windowTitle := maybe "<stdin>" id infile]
  onDestroy win mainQuit

  sw <- scrolledWindowNew Nothing Nothing
  set win [containerChild := sw]

  (store,view) <- createTable cols cnames ldata
  set sw [containerChild := view]

  widgetShowAll win
  mainGUI


parseDataOrExit sourceName sep input = 
    case parseSV sourceName sep input of
      Left err -> do
        print err
        exitFailure
        return undefined
      Right rs -> return rs


mkRow :: [a] -> Array Int a
mkRow vs = listArray (1,n) vs where
    n  = length vs

-- mkRowIO :: [String] -> IO (IOArray Int String)
-- mkRowIO vs = newListArray (1,n) vs where n = length vs

arraySize a = let (l,h) = bounds a in 1 + h - l

ixDefault def a i = if l <= i && i <= h then a!i else def where
    (l,h) = bounds a

safeIndex a i = if l <= i && i <= h then Just (a!i) else Nothing
    where (l,h) = bounds a

createTable cols columnNames tableData = do
  store <- listStoreNew tableData -- $ map (fmap valueToString) tableData
  sorted <- treeModelSortNewWithModel store
  view <- treeViewNewWithModel sorted
  view `set` [treeViewEnableGridLines := TreeViewGridLinesHorizontal]

  -- Add each column
  forM_ [1..cols] $ \i -> do
    let columnName = ixDefault [] columnNames i
    col <- createSortedColumn store sorted i columnName
           $ \tr -> [cellText := maybe [] valueToString (safeIndex tr i)]
    treeViewAppendColumn view col

  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    iter <- treeModelSortConvertIterToChildIter sorted iter
    row <- treeModelGetRow store iter
    return $ any (isPrefixOf str . valueToString) $ elems row

  return (store, view)


createSortedColumn store sortedStore colId title f = do
  col <- treeViewColumnNew
  set col [treeViewColumnTitle := title]
  treeViewColumnSetResizable col True

  cell <- cellRendererTextNew
  cell `set` [cellTextEditable := True]
  cellLayoutPackStart col cell False
  cellLayoutSetAttributes col cell store f

  treeViewColumnSetSortColumnId col colId
  treeSortableSetSortFunc sortedStore colId $ \l r -> do
    x <- treeModelGetRow store l
    y <- treeModelGetRow store r
    return (compare (cellString x colId) (cellString y colId))

  return col


cellString row = maybe [] valueToString . safeIndex row
