module Sheet where

import qualified Data.Map as Map

type TextCell = String
type NumberCell = Int

data Sheet = Sheet {
    name :: String,
    width :: Int,
    height :: Int,
    cells :: Map.Map (Int, Int) Cell
}

data Cell = TextCell { str :: String } | NumberCell { val :: Int } deriving (Show, Eq)

set :: Sheet -> Int -> Int -> Cell -> Sheet

set sheet row column value = Sheet (getName sheet) (getWidth sheet) (getHeight sheet) (Map.insert (row, column) value (getCells sheet))

get :: Sheet -> Int -> Int -> IO ()

get sheet row column = do
    let result = Map.lookup (row, column) (getCells sheet)
    case result of
        Nothing -> putStrLn "not found"
        Just x -> putStrLn "found"

getCells :: Sheet -> (Map.Map (Int, Int) Cell)
getCells (Sheet name width height cells) = cells

getName :: Sheet -> String
getName (Sheet name width height cells) = name

getWidth :: Sheet -> Int
getWidth (Sheet name width height cells) = width

getHeight :: Sheet -> Int
getHeight (Sheet name width height cells) = height