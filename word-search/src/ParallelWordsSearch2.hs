module ParallelWordsSearch2 (findWordsParallel) where

import Data.List (nub)
import Control.Parallel.Strategies
import qualified SequentialSearch

findWordsParallel :: Int -> [[Char]] -> [String] -> [String]
findWordsParallel splits board wordsList =
    let subBoards = splitBoard splits board
        trie = foldr SequentialSearch.insertWord SequentialSearch.emptyTrie wordsList
        results = parMap rdeepseq (\subBoard -> findWordsTrie subBoard trie) subBoards
    in nub (concat results)

findWordsTrie :: [[Char]] -> SequentialSearch.Trie -> [String]
findWordsTrie board trie = nub $ concatMap (\(r,c) -> 
    SequentialSearch.searchFromCell board trie r c []
    ) [(r,c) | r <- [0..rows-1], c <- [0..cols-1]]
  where
    rows = length board
    cols = length (head board)

splitBoard :: Int -> [[Char]] -> [[[Char]]]
splitBoard n board
    | n <= 1 = [board]
    | n >= rows = [extractSubBoard board r (r + 1) c (c + 1) | r <- [0..rows-1], c <- [0..min rows cols-1]]
    | n >= cols = [extractSubBoard board r (r + 1) c (c + 1) | r <- [0..min rows cols-1], c <- [0..cols-1]]
    | otherwise =
        [extractSubBoard board (r * subRows) ((r + 1) * subRows) (c * subCols) ((c + 1) * subCols)
         | r <- [0..n-1], c <- [0..n-1]]
  where
    rows = length board
    cols = length (head board)
    subRows = rows `div` n
    subCols = cols `div` n 

extractSubBoard :: [[Char]] -> Int -> Int -> Int -> Int -> [[Char]]
extractSubBoard board rowStart rowEnd colStart colEnd =
    [take (colEnd - colStart) . drop colStart $ row | row <- take (rowEnd - rowStart) . drop rowStart $ board]
