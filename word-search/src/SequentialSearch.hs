module SequentialSearch where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.Char (isAlpha)
import System.IO

-- Trie data structure
data Trie = Trie {
    children :: Map.Map Char Trie,
    isEnd :: Bool
} deriving (Show)

-- Create empty trie
emptyTrie :: Trie
emptyTrie = Trie Map.empty False

-- Insert a word into trie
insertWord :: String -> Trie -> Trie
insertWord "" trie = trie { isEnd = True }
insertWord (c:cs) trie = 
    let childTrie = fromMaybe emptyTrie (Map.lookup c (children trie))
        newChild = insertWord cs childTrie
    in trie { children = Map.insert c newChild (children trie) }

-- Main search function
findWords :: [[Char]] -> [String] -> [String]
findWords board words = nub $ concatMap (\(r,c) -> 
    searchFromCell board trie r c []
    ) [(r,c) | r <- [0..rows-1], c <- [0..cols-1]]
  where
    rows = length board
    cols = length (head board)
    trie = foldr insertWord emptyTrie words

-- Search from a specific cell
searchFromCell :: [[Char]] -> Trie -> Int -> Int -> String -> [String]
searchFromCell board trie row col currWord
    | row < 0 || row >= rows || col < 0 || col >= cols = []
    | board !! row !! col == '*' = []  -- Check for visited cell
    | not (Map.member curr (children trie)) = []
    | otherwise = 
        let newTrie = fromMaybe emptyTrie (Map.lookup curr (children trie))
            newWord = currWord ++ [curr]
            foundWords = [newWord | isEnd newTrie]
            markedBoard = markCell board row col
            nextWords = concatMap (\(dr,dc) -> 
                searchFromCell markedBoard newTrie (row+dr) (col+dc) newWord
                ) [(0,1), (1,0), (0,-1), (-1,0)]
        in foundWords ++ nextWords
  where
    rows = length board
    cols = length (head board)
    curr = board !! row !! col

-- Mark/unmark cell (replace with temporary character)
markCell :: [[Char]] -> Int -> Int -> [[Char]]
markCell board row col = 
    take row board ++
    [take col (board !! row) ++ ['*'] ++ drop (col+1) (board !! row)] ++
    drop (row+1) board

