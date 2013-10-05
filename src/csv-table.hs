import Control.Monad
import Data.Array
import Data.Maybe
import Data.List       (isPrefixOf)
import Data.List.Split (splitOn)
import Graphics.UI.Gtk
import System.Environment (getArgs)

defaultSep = "\t"

main = do

  -- Get args etc
  args <- getArgs
  ls <- lines `fmap` getContents
  let sep = maybe defaultSep id $ listToMaybe args

  -- Setup data
  let ldata = map (mkRow sep) ls
  let cols  = maximum $ map arraySize ldata

  -- Start GUI
  initGUI
  win <- windowNew
  onDestroy win mainQuit

  (store,view) <- createTable cols ldata
  set win [containerChild := view]

  widgetShowAll win
  mainGUI


mkRow :: String -> String -> Array Int String
mkRow sep s = listArray (1,n) ss where
    n  = length ss
    ss = splitOn sep s

arraySize a = let (l,h) = bounds a in 1 + h - l


createTable cols tableData = do
  store <- listStoreNew tableData
  sorted <- treeModelSortNewWithModel store
  view <- treeViewNewWithModel sorted

  -- Add each column
  forM_ [1..cols] $ \i -> do
    col <- createSortedColumn store sorted i (show i) $ \tr -> [cellText := (tr!i)]
    treeViewAppendColumn view col

  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    iter <- treeModelSortConvertIterToChildIter sorted iter
    row <- treeModelGetRow store iter
    return $ any (isPrefixOf str) $ elems row

  return (store, view)
  where
    eqf s it = return False -- TODO


createSortedColumn store sortedStore colId title f = do
  col <- treeViewColumnNew
  set col [treeViewColumnTitle := title]
  treeViewColumnSetSortColumnId col colId

  cell <- cellRendererTextNew
  cellLayoutPackStart col cell False
  cellLayoutSetAttributes col cell store f

  treeSortableSetSortFunc sortedStore colId $ \l r -> do
    x <- treeModelGetRow store l
    y <- treeModelGetRow store r
    return (compare (x!colId) (y!colId))

  return col
