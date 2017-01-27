module FileOperations (
        load,
        save,
) where

import Sheet
import Data.List.Split
import Data.Sequence as DS
import Data.Foldable as DF
import Data.List as DL

colSeparator = "\t"
rowSeparator = "\n"

save :: Sheet -> String -> IO Sheet
save sheet file = do
    writeFile file (encode sheet)
    return sheet

load :: String -> IO Sheet
load file = do
    str <- readFile file
    return (decode str)

encode :: Sheet -> String
encode (Sheet name width height cells) = intercalate rowSeparator ([name, (show width), (show height)]++(encodeCells cells))

encodeCells :: Seq (Seq Cell) -> [String]
encodeCells cells = map encodeRow (DF.toList cells)

encodeRow :: Seq Cell -> String
encodeRow row = intercalate colSeparator (map encodeCell (DF.toList row))

decode :: String -> Sheet
decode str =
      let parts = splitOn rowSeparator str
      in Sheet (parts !! 0) (read (parts !! 1) :: Int) (read (parts !! 2)) (decodeCells (Prelude.drop 3 parts))

decodeCells :: [String] -> Cells
decodeCells rows = DS.fromList(map decodeRow rows)

decodeRow :: String -> Seq Cell
decodeRow rowStr = DS.fromList(map decodeCell (splitOn colSeparator rowStr))
