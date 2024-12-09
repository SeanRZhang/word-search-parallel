module InputParser (parseBoard, parseWords) where

import Data.Char (isAlpha)

-- Parse a single cell content (e.g., "a" -> 'a')
parseCell :: String -> Char
parseCell s = 
    case filter isAlpha s of
        (c:_) -> c
        [] -> error $ "Invalid cell content: " ++ s

-- Parse input string to get board
parseBoard :: String -> [[Char]]
parseBoard input = 
    let content = init $ tail input  -- Remove outer brackets
        rows = splitRows content
        parsedRows = map parseRow rows
    in parsedRows
  where
    splitRows :: String -> [String]
    splitRows [] = []
    splitRows s = 
        let (row, rest) = break (==']') (dropWhile (/='[') s)
        in if null rest 
           then []
           else (tail row) : splitRows (tail rest)

    parseRow :: String -> [Char]
    parseRow s = map parseCell (splitCells s)
    
    splitCells :: String -> [String]
    splitCells [] = []
    splitCells s = 
        let (cell, rest) = break (==',') (dropWhile (not . isAlpha) s)
        in if null cell 
           then splitCells rest
           else cell : splitCells rest

-- Parse input string to get words
parseWords :: String -> [String]
parseWords input = 
    let content = init $ tail input  -- Remove outer brackets
        wordsList = splitWords content
    in map (filter isAlpha) wordsList
  where
    splitWords :: String -> [String]
    splitWords [] = []
    splitWords s = 
        let (word, rest) = break (==',') (dropWhile (not . isAlpha) s)
        in if null word 
           then splitWords rest
           else word : splitWords rest
