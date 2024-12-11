module ParallelSubgridSearch (findWordsSubgrids) where

import Data.List (nub)
import Control.Parallel.Strategies
import qualified SequentialSearch

findWordsSubgrids :: Int -> [[Char]] -> [String] -> [String]
findWordsSubgrids splits board wordsList =
    let subBoards = splitBoard splits board
        trie = foldr SequentialSearch.insertWord SequentialSearch.emptyTrie wordsList
        results = parMap rdeepseq (\subBoard -> findWordsTrie board trie subBoard) subBoards
    in nub (concat results)

findWordsTrie :: [[Char]] -> SequentialSearch.Trie -> (Int, Int, Int, Int)-> [String]
findWordsTrie board trie (rStart, rEnd, cStart, cEnd) = 
    nub $ concatMap (\(r,c) -> 
        SequentialSearch.searchFromCell board trie r c []
    ) [(r,c) | r <- [rStart..min rEnd (rows-1)], c <- [cStart..min cEnd (cols-1)]]
  where
    rows = length board
    cols = length (head board)

splitBoard :: Int -> [[Char]] -> [(Int, Int, Int, Int)]
splitBoard n board
    | n <= 1 = [(0, rows, 0, cols)]
    | n >= rows = [(r, r + 1, c, c + 1) | r <- [0..rows-1], c <- [0..min rows cols-1]]
    | n >= cols = [(r, r + 1, c, c + 1) | r <- [0..min rows cols-1], c <- [0..cols-1]]
    | otherwise =
        [(r * subRows, (r + 1) * subRows, c * subCols, (c + 1) * subCols)
         | r <- [0..n-1], c <- [0..n-1]]
  where
    rows = length board
    cols = length (head board)
    subRows = rows `div` n
    subCols = cols `div` n 
