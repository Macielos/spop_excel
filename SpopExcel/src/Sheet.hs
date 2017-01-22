module Sheet
{-
}(
newSheet,
set,
setFunc,
clear,
printSheet,
Sheet(..)
{-
expandSheetIfNecessary,
getCells,
getName,
getWidth,
getHeight,
-}
) -}where

import Data.Sequence as DS
import Data.Foldable as DF
import Data.List as DL

data Sheet = Sheet {
    name :: String,
    width :: Int,
    height :: Int,
    cells :: Cells
} deriving (Show, Eq)

type Cells = Seq (Seq Cell)

data Cell =
    Empty |
    TextCell {str :: String} |
    NumberCell {value :: Float} |
    SumCell {range :: Range} |
    ProductCell {range :: Range} |
    MeanCell {range :: Range}
    deriving (Show, Eq)

data Range = Range {
    x1 :: Int,
    y1 :: Int,
    x2 :: Int,
    y2 :: Int
} deriving (Show, Eq)

getValue :: Sheet -> Cell -> String
getValue _ Empty = ""
getValue _ (TextCell str) = str
getValue sheet cell = show (getNumber sheet 0.0 cell)

getNumber :: Sheet -> Float -> Cell -> Float
getNumber _ def Empty = def
getNumber _ def (TextCell n) = def
getNumber _ _ (NumberCell n) = n
getNumber sheet _ (SumCell range) = calcSum sheet range
getNumber sheet _ (ProductCell range) = calcProduct sheet range
getNumber sheet _ (MeanCell range) = calcMean sheet range

calcSum :: Sheet -> Range -> Float
calcSum sheet (Range x1 y1 x2 y2) = foldr (+) 0.0 (map (calcSumInternal x1 x2 sheet) (DF.toList(DS.drop y1 (DS.take (y2+1) (getCells sheet)))))

calcSumInternal :: Int -> Int -> Sheet -> Seq Cell -> Float
calcSumInternal x1 x2 sheet row = foldr (+) 0.0 (map (getNumber sheet 0.0) (DF.toList (DS.drop x1 (DS.take (x2+1) row))))

calcProduct :: Sheet -> Range -> Float
calcProduct sheet (Range x1 y1 x2 y2) = foldr (*) 1.0 (map (calcProductInternal x1 x2 sheet) (DF.toList(DS.drop y1 (DS.take (y2+1) (getCells sheet)))))

calcProductInternal :: Int -> Int -> Sheet -> Seq Cell -> Float
calcProductInternal x1 x2 sheet row = foldr (*) 1.0 (map (getNumber sheet 1.0) (DF.toList (DS.drop x1 (DS.take (x2+1) row))))

calcMean :: Sheet -> Range -> Float
calcMean sheet (Range x1 y1 x2 y2) = (calcSum sheet (Range x1 y1 x2 y2)) / fromIntegral((1 + x2 - x1) * (1 + y2 - y1))

{-TODO
sformatowac to sensownie,
getCells printowac w formie tabeli,
dla typow sum, product, mean obliczac je przed wyswietleniem, moze niech implementuja jakis typ z funkcja getValue albo cos
-}
printSheet :: Sheet -> IO Sheet
printSheet sheet = do putStrLn ((getName sheet)++" ["++(show (getWidth sheet))++"x"++(show (getHeight sheet))++"] ")
                      putStr (printCells sheet)
                      return sheet

printCells :: Sheet -> String
printCells sheet = concat (map (printRow sheet) (DF.toList (getCells sheet)))

printRow :: Sheet -> Seq Cell -> String
printRow sheet row = concat (DL.intersperse "\t|" (map (getValue sheet) (DF.toList row))) ++ "\n"

printCellDetails :: Sheet -> Int -> Int -> IO Sheet
printCellDetails sheet x y = do putStrLn (show (DS.index (DS.index (getCells sheet) y) x))
                                return sheet

set :: Sheet -> Int -> Int -> String -> Sheet
set sheet x y value = setCell sheet x y (newCell value)

setFunc :: Sheet -> Int -> Int -> String -> Int -> Int -> Int -> Int -> Sheet
setFunc sheet x y function x1 y1 x2 y2 = setCell sheet x y (newFuncCell function x1 y1 x2 y2)

clear :: Sheet -> Int -> Int -> Sheet
clear sheet x y = setCell sheet x y Empty

setCell :: Sheet -> Int -> Int -> Cell -> Sheet
setCell sheet x y value =
    Sheet (getName sheet) (max (x+1) (getWidth sheet)) (max (y+1) (getHeight sheet))
    (setCellInternal (expandSheetIfNecessary (getCells sheet) x y) x y value)

setCellInternal :: Cells -> Int -> Int -> Cell -> Cells
setCellInternal cells x y value = DS.update y (DS.update x value (index cells y)) cells




{-TODO pewnie mozna to lepiej zrobic-}
newCell :: String -> Cell
newCell value =
    if value == ""
    then Empty
    else case reads value :: [(Float, String)] of
              [(_, "")] -> NumberCell (read value :: Float)
              _         -> TextCell value

newFuncCell :: String -> Int -> Int -> Int -> Int -> Cell
newFuncCell function x1 y1 x2 y2 = newFuncCellRange function (Range x1 y1 x2 y2)

newFuncCellRange :: String -> Range -> Cell
newFuncCellRange function range = case function of
    "sum" -> SumCell range
    "product" -> ProductCell range
    "mean" -> MeanCell range
    _      -> Empty

newSheet :: String -> Sheet
newSheet name = Sheet name 1 1 (DS.singleton(newRow 1))

newRow :: Int -> Seq Cell
newRow length = DS.fromList(map toEmpty [x | x <-[0..(length-1)]])

toEmpty a = Empty



expandSheetIfNecessary :: Cells -> Int -> Int -> Cells
expandSheetIfNecessary cells x y = addRowsIfNecessary (addColumnsIfNecessary cells x) y

addRowsIfNecessary :: Cells -> Int -> Cells
addRowsIfNecessary cells y =
    if (DS.length cells) < (y + 1)
    then addRowsIfNecessary (cells |> newRow (DS.length (index cells 0))) y
    else cells

addColumnsIfNecessary :: Cells -> Int -> Cells
addColumnsIfNecessary cells x =
    if (DS.length (index cells 0)) < (x + 1)
    then expandRow cells 0 (x + 1 - DS.length (index cells 0))
    else cells

expandRow :: Cells -> Int -> Int -> Cells
expandRow cells rowNr elementsToAdd =
    if (rowNr < DS.length cells)
    then expandRow (DS.update rowNr ((index cells rowNr) >< newRow elementsToAdd) cells) (rowNr+1) elementsToAdd
    else cells




getCells :: Sheet -> Seq (Seq Cell)
getCells (Sheet name width height cells) = cells

getName :: Sheet -> String
getName (Sheet name width height cells) = name

getWidth :: Sheet -> Int
getWidth (Sheet name width height cells) = width

getHeight :: Sheet -> Int
getHeight (Sheet name width height cells) = height