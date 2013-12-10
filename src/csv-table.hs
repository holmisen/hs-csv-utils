import qualified Args
import ParseSV
import Record
import Utils

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Array
import Data.Array.IO
import Data.Maybe
import Data.List       (isPrefixOf)
import Data.List.Split (splitOn)
import Graphics.UI.Gtk
import System.Exit     (exitFailure)


main = do

  -- Get args etc
  args <- Args.getArgs
  let header = isJust $ Args.lookup "-h" args
  let infile = listToMaybe $ Args.rest args
  let sep    = fmap head (Args.lookup "-d" args)
               <|> join (fmap sepFromFileExtension infile)
               <|> defaultSep
  
  -- Read data
  input <- maybe getContents readFile infile

  -- Parse data
  records <- parseDataOrExit [] sep input
  let ldata :: [Record]
      ldata  = if header then tail records else records
  let cols   = maxRecordWidth records
  let cnames :: Array Int String
      cnames = if header then arrayFromList (map unparseValue $ recordValues $ head records) else listArray (1,cols) $ map show [1..cols]

  -- Start GUI
  initGUI
  win <- windowNew
  win `set` [windowTitle := maybe "<stdin>" id infile]
  onDestroy win mainQuit

  sw <- scrolledWindowNew Nothing Nothing
  set win [containerChild := sw]

  (store,view) <- createTable cols cnames ldata

  set sw [ containerChild := view
         , scrolledWindowHscrollbarPolicy := PolicyAutomatic
         , scrolledWindowVscrollbarPolicy := PolicyAutomatic ]

  -- Make sensible default size
  Requisition reqWidth reqHeight <- widgetSizeRequest view
  windowSetDefaultSize win reqWidth reqHeight

  widgetShowAll win
  mainGUI


parseDataOrExit :: String -> Char -> String -> IO [Record]
parseDataOrExit sourceName sep input = 
    case parseRecords sourceName sep input of
      Left err -> do
        print err
        exitFailure
        return undefined
      Right rs -> return rs


mkRow :: [a] -> Array Int a
mkRow vs = listArray (1,n) vs where
    n  = length vs

safeIndex a i = if l <= i && i <= h then Just (a!i) else Nothing
    where (l,h) = bounds a

createTable cols columnNames tableData = do
  store <- listStoreNew tableData -- $ map (fmap valueToString) tableData
  sorted <- treeModelSortNewWithModel store
  view <- treeViewNewWithModel sorted
--  set view [treeViewEnableGridLines := TreeViewGridLinesHorizontal]
  set view [treeViewHoverSelection := True]
  set view [treeViewRulesHint := True]

  -- Add each column
  forM_ [1..cols] $ \i -> do
    let columnName = ixDefault [] columnNames i
    col <- createSortedColumn store sorted i columnName
           $ \tr -> [cellText := cellString tr i]
    treeViewAppendColumn view col

  -- Enable searching
  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    iter <- treeModelSortConvertIterToChildIter sorted iter
    row <- treeModelGetRow store iter
    return $ any (isPrefixOf str . unparseValue) $ elems row

  -- -- Enable multiple selection
  -- sel <- treeViewGetSelection view
  -- treeSelectionSetMode sel SelectionMultiple

  -- -- Enable copy
  -- -- TODO
  -- -- Must disable treeViewHoverSelection when implementing this.
  -- on view keyPressEvent $ tryEvent $ do
  --   [Control] <- eventModifier
  --   "c" <- eventKeyName
  --   liftIO $ putStrLn "Ctrl-C"

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


cellString row = maybe [] unparseValue . safeIndex row

