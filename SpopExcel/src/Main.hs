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
                   then Left $ return "Too few arguments for function 'clear'"
                   else if not ((isInt (parts !! 1)) && (isInt (parts !! 2)))
                   then Left $ return "Error - input Integer only"
                   else if not (checkBoundaries sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                   then Left $ return "Coodintates beyond range"
                   else Right $ return (clear sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
        "get" -> do
                 if (P.length parts < 3)
                 then Left $ return "Too few arguments for function 'get'"
                 else if not ((isInt (parts !! 1)) && (isInt (parts !! 2)))
                 then Left $ return "Error - input Integer only"
                 else if not (checkBoundaries sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                 then Left $ return "Coodintates beyond range"
                 else Right $ (printCellDetails sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
        "set" -> do
                 if (P.length parts < 4)
                 then Left $ return "Too few arguments for function 'set'"
                 else if not ((isInt (parts !! 1)) && (isInt (parts !! 2)))
                 then Left $ return "Error - input Integer only"
                 else if not (checkBoundaries sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                 then Left $ return "Coodintates beyond range"
                 else Right $ return (set sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3))
        "setFunc" -> do
                         if (P.length parts < 8)
                         then Left $ return "Too few arguments for function 'setFunc'"
                         else if not ((isInt (parts !! 1)) && (isInt (parts !! 2)) && (isInt (parts !! 4))&& (isInt (parts !! 5))&& (isInt (parts !! 6))&& (isInt (parts !! 7)))
                         then Left $ return "Error - input Integer only"
                         else if not (checkBoundaries sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int))
                         then Left $ return "Coodintates beyond range"
                         else if not (checkRange sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (read (parts !! 4) :: Int) (read (parts !! 5) :: Int) (read (parts !! 6) :: Int) (read (parts !! 7) :: Int))
                         then Left $ return "Range invalid or beyond sheet boundaries"
                         else if not (checkFunc sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (read (parts !! 4) :: Int) (read (parts !! 5) :: Int) (read (parts !! 6) :: Int) (read (parts !! 7) :: Int))
                         then Left $ return "Cyclic dependency detected"
                         else Right $ return (setFunc sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3) (read (parts !! 4) :: Int) (read (parts !! 5) :: Int) (read (parts !! 6) :: Int) (read (parts !! 7) :: Int))
        "new" -> do
                      if (P.length parts == 2)
                      then Right $ return (newSheet (parts !! 1))
                      else Left $ return "Too few arguments for function 'new'"
        "rename" -> do
                  if (P.length parts == 2)
                  then Right $ return (renameSheet sheet (parts !! 1))
                  else Left $ return "Too few arguments for function 'rename'"
        "save" -> do
                    if (P.length parts == 2)
                    then Right $ (save sheet (parts !! 1))
                    else Left $ return "Too few arguments for function 'save'"
        "load" -> do
                    if (P.length parts == 2)
                    then Right $ (load (parts !! 1))
                    else Left $ return "Too few arguments for function 'load'"
        otherwise -> Left $ return "incorrect command, type 'help' for available commands"


isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = (isDigit x) && (isInt xs)

printHelp :: String
printHelp = "Available commands:\n1. set X Y value (number or string) \n2. get X Y\n3. clear X Y\n4. print\n5. setFunc X Y sum | product | mean X1 Y1 X2 Y2 (range boundaries)\n6. new NAME\n7. rename NAME\n8. save FILENAME\n9. load FILENAME\n10. help\n11. quit"

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