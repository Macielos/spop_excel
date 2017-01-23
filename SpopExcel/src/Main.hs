module Main where

import Sheet
import FileOperations
import System.IO
import Data.Sequence as DS
import Data.Char
import Data.Foldable
import Prelude

process :: [String] -> Sheet -> Either (IO String) (IO Sheet)
process parts sheet =
    if (Prelude.length parts < 1)
    then Left $ return ""

    else case (parts !! 0) of
        "quit" -> Left $ return "quit"
        "help" -> Left $ return printHelp
        "print" -> Right $ (printSheet sheet)
        "clear" -> do
                     if ((isInt (parts !! 1)) && (isInt (parts !! 2)) == True)
                     then Right $ return (clear sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                     else Left $ return "Error - input Integer only"
        "get" ->  do
                    if ((isInt (parts !! 1)) && (isInt (parts !! 2)) == True)
                    then Right $ (printCellDetails sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                    else Left $ return "Error - input Integer only"
        "set" -> do
                   if ((isInt (parts !! 1)) && (isInt (parts !! 2)) == True)
                   then Right $ return (set sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3))
                   else Left $ return "Error - input Integer only"
        "setFunc" -> do
                       if ((isInt (parts !! 1)) && (isInt (parts !! 2)) && (isInt (parts !! 4)&& (isInt (parts !! 5)&& (isInt (parts !! 6)&& (isInt (parts !! 7))== True)
                       then Right $ return (setFunc sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3) (read (parts !! 4) :: Int) (read (parts !! 5) :: Int) (read (parts !! 6) :: Int) (read (parts !! 7) :: Int))
                       else Left $ return "Error - input Integer only"
        otherwise -> Left $ return "incorrect command, type 'help' for available commands"
        --TODO newSheet, rename, save, open


isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = (isDigit x) && (isInt xs)

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