module Main where

import Sheet
import FileOperations
import System.IO
import Data.Sequence as DS
import Data.Char
import Data.Foldable
import Prelude as P

process :: [String] -> Sheet -> Either (IO String) (IO Sheet)
process parts sheet =
    if (P.length parts < 1)
    then Left $ return ""

    else case (parts !! 0) of
        "quit" -> Left $ return "quit"
        "help" -> Left $ return printHelp
        "print" -> Right $ (printSheet sheet)
        "clear" -> do
                   if (P.length parts < 3)
                   then Left $ return "A few arguments for function 'clear'"
                   else if ((isInt (parts !! 1)) && (isInt (parts !! 2)) == True)
                   then Right $ return (clear sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                   else Left $ return "Error - input Integer only"
        "get" -> do
                 if (P.length parts < 3)
                 then Left $ return "A few arguments for function 'get'"
                 else if ((isInt (parts !! 1)) && (isInt (parts !! 2)) == True)
                 then Right $ (printCellDetails sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                 else Left $ return "Error - input Integer only"
        "set" -> do
                 if (P.length parts < 4)
                 then Left $ return "A few arguments for function 'set'"
                 else if ((isInt (parts !! 1)) && (isInt (parts !! 2)) == True)
                 then Right $ return (set sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3))
                 else Left $ return "Error - input Integer only"
        "setFunc" -> do
                     if (P.length parts < 8)
                     then Left $ return "A few arguments for function 'setFunc'"
                     else if ((isInt (parts !! 1)) && (isInt (parts !! 2)) && (isInt (parts !! 4))&& (isInt (parts !! 5))&& (isInt (parts !! 6))&& (isInt (parts !! 7))== True)
                     then Right $ return (setFunc sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3) (read (parts !! 4) :: Int) (read (parts !! 5) :: Int) (read (parts !! 6) :: Int) (read (parts !! 7) :: Int))
                     else Left $ return "Error - input Integer only"
        otherwise -> Left $ return "incorrect command, type 'help' for available commands"
        --TODO newSheet, rename, save, open


isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = (isDigit x) && (isInt xs)

printHelp :: String
printHelp = "Avalible commands:\n 1.set - arguments X Y value\n 2.get - arguments X Y\n 3.clear - arguments X Y\n 4.print - display current sheet\n 5.setFunc - ???????????\n 6.help\n 7.quit"

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (P.drop 1 y) where (x,y) = span (/= d) s

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