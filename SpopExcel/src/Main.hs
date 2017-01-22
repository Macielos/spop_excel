module Main where

import Sheet
import FileOperations
import System.IO
import Data.Sequence as DS

process :: [String] -> Sheet -> Either (IO String) (IO Sheet)
process parts sheet =
    if Prelude.length parts < 1
    then Left $ return ""
    {-TODO
    - walidacja parametrow
    - przed zawolaniem setFunc sprawdzic czy w range sa tylko komorki number lub empty
    w sumie przed wstawieniem komorki text lub func tez trzeba by sprawdzac czy nie jest w przedziale funkcji jakiejs innej komorki
    bo inaczej mogloby sie zawolac liczenie np sredniej ze stringow (ale to mozna i pomijac) albo sredniej z pol gdzie jest funkcja
    wtedy trzeba by sprawdzac czy nie dojdzie do nieskonczonej rekursji
    -}
    else case (parts !! 0) of
        "quit" -> Left $ return "quit"
        "help" -> Left $ return printHelp
        "print" -> Right $ (printSheet sheet)
        "clear" -> Right $ return (clear sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
        "get" -> Right $ (printCellDetails sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
        "set" -> Right $ return (set sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3))
        "setFunc" -> Right $ return (setFunc sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3) (read (parts !! 4) :: Int) (read (parts !! 5) :: Int) (read (parts !! 6) :: Int) (read (parts !! 7) :: Int))
        otherwise -> Left $ return "incorrect command, type 'help' for available commands"
        --TODO newSheet, rename, save, open

printHelp :: String
printHelp = "[tu bedzie help]\ndruga linia\ntrzecia linia"

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (Prelude.drop 1 y) where (x,y) = span (/= d) s

readCommand :: IO String
readCommand = do
    putStr "> "
    getLine

mainLoop sheet = do
              command <- readCommand
              let parts = split ' ' command
              let result = process parts sheet
              case result of
                  Left e -> do
                            response <- e
                            if response == "quit"
                            then putStrLn "quitting..."
                            else if response == ""
                            then do
                                 mainLoop sheet
                            else do
                                 putStrLn response
                                 mainLoop sheet
                  Right s -> do
                            sheet <- s
                            mainLoop sheet

haskellExcel = let sheet = newSheet "the sheet has no name"
               in do putStrLn "Welcome to haskell spreadsheet "
                     putStrLn "Type 'help' for available commands "
                     mainLoop sheet

main = haskellExcel