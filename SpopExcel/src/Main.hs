module Main where

import Sheet
import System.IO
import Data.Sequence as DS

process :: [String] -> Sheet -> Either (IO String) (IO Sheet)
process parts sheet = case (parts !! 0) of
    "quit" -> Left $ return "quit"
    "print" -> Right $ (printSheet sheet)
    "addRow" -> Right $ return (addRow sheet)
    "setInt" -> Right $ return (setInt sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (read (parts !! 3) :: Int))
    "setString" -> Right $ return (setString sheet (read (parts !! 1) :: Int) (read (parts !! 2) :: Int) (parts !! 3))
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
                            x <- e
                            if x == "quit"
                            then putStrLn "quitting..."
                            else do
                                 putStrLn "zjebalo sie bo jest error "
                                 mainLoop sheet
                  Right s -> do
                            sheet <- s
                            putStrLn "sheet wciaz dycha"
                            mainLoop sheet


haskellExcel = let sheet = Sheet "testsheet" 0 0 DS.empty
               in do putStrLn "Witaj w haskellowym arkuszu kalkulacyjnym: "
                     mainLoop sheet

main = haskellExcel