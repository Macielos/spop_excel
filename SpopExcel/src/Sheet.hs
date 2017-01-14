module Sheet where

import Data.Sequence as DS

data Sheet = Sheet {
    name :: String,
    width :: Int,
    height :: Int,
    cells :: Seq (Seq Cell)
}

newRow :: Seq Cell
newRow = DS.fromList([Empty, Empty, Empty])

data Cell = Empty | TextCell {str :: String} | NumberCell {value :: Int} deriving (Show, Eq)

printSheet :: Sheet -> IO Sheet
printSheet sheet = do print (getName sheet)
                      print (getCells sheet)
                      return sheet

addRow :: Sheet -> Sheet
addRow sheet = Sheet (getName sheet) (getWidth sheet) (getHeight sheet + 1) ((getCells sheet) |> newRow)

setInt :: Sheet -> Int -> Int -> Int -> Sheet
setInt sheet row column value = Sheet (getName sheet) (getWidth sheet) (getHeight sheet) (DS.update row (DS.update column (NumberCell value) (index (getCells sheet) row)) (getCells sheet))

setString :: Sheet -> Int -> Int -> String -> Sheet
setString sheet row column str = Sheet (getName sheet) (getWidth sheet) (getHeight sheet) (DS.update row (DS.update column (TextCell str) (index (getCells sheet) row)) (getCells sheet))


{-
set :: Sheet -> Int -> Int -> Cell -> Sheet

set sheet row column value = Sheet (getName sheet) (getWidth sheet) (getHeight sheet) (Map.insert (row, column) value (getCells sheet))

get :: Sheet -> Int -> Int -> IO ()

get sheet row column = do
    let result = Map.lookup (row, column) (getCells sheet)
    case result of
        Nothing -> putStrLn "not found"
        Just x -> putStrLn "found"
--}
getCells :: Sheet -> Seq (Seq Cell)
getCells (Sheet name width height cells) = cells

getName :: Sheet -> String
getName (Sheet name width height cells) = name

getWidth :: Sheet -> Int
getWidth (Sheet name width height cells) = width

getHeight :: Sheet -> Int
getHeight (Sheet name width height cells) = height