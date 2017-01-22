module FileOperations (
        loadSheet,
        saveSheet,
) where

import Data.List
import Data.List.Split
import System.Directory (removeFile)

tmpFile = "sheet.tmp"
dataDir = ""
colSeparator = "\t"
rowSeparator = "\n"

loadSheet :: FilePath -> IO [[String]]
loadSheet name = do
        content <- readFile (dataDir ++ name)
        -- File content is saved to temporary file to force reading file (due to leazy evaluation) and closing its handle
        writeFile tmpFile content
        removeFile tmpFile
        return (decode content)

saveSheet :: FilePath -> [[String]] -> IO ()
saveSheet name content = do
        writeFile (dataDir ++ name) (encode content)
        return ()

decode :: String -> [[String]]
decode str =
        let
                filteredStr = filter (\c -> c /= '\r') str
                decodeLine line = splitOn colSeparator line
        in map decodeLine $ splitOn rowSeparator filteredStr

encode :: [[String]] -> String
encode mat =
        let
                encodeLine arr = intercalate colSeparator arr
        in intercalate rowSeparator $ map encodeLine $ mat