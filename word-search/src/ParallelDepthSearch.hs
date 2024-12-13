module ParallelDepthSearch (findWords, insertWord, Trie(..), emptyTrie, searchFromCell) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Parallel.Strategies (parMap, rseq)


data Trie = Trie {
    children :: Map.Map Char Trie,  
    isEnd :: Bool                   
} deriving (Show)


emptyTrie :: Trie
emptyTrie = Trie Map.empty False

insertWord :: String -> Trie -> Trie
insertWord "" trie = trie { isEnd = True }
insertWord (c:cs) trie = 
    let childTrie = fromMaybe emptyTrie (Map.lookup c (children trie))
        newChild = insertWord cs childTrie
    in trie { children = Map.insert c newChild (children trie) }

findWords :: Int -> [[Char]] -> [String] -> [String]
findWords depth board targetWords = 
    nub $ concat $ parMap rseq (\(r, c) -> searchFromCell board trie Set.empty r c [] depth 0 rows cols)  
    [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
  where
    rows = length board
    cols = length (head board)
    trie = foldr insertWord emptyTrie targetWords  

searchFromCell :: [[Char]] -> Trie -> Set (Int, Int) -> Int -> Int -> String -> Int -> Int -> Int -> Int -> [String]
searchFromCell board trie visited row col currWord depth level rows cols
    | row < 0 || row >= rows || col < 0 || col >= cols = [] 
    | Set.member (row, col) visited = []  
    | not (Map.member curr (children trie)) = []  
    | otherwise = 
        let newTrie = fromMaybe emptyTrie (Map.lookup curr (children trie))  
            newWord = currWord ++ [curr]  
            foundWords = [newWord | isEnd newTrie]  
            newVisited = Set.insert (row, col) visited  

            nextWords = if level < depth  
                        then concat $ parMap rseq (\(r, c) -> searchFromCell board newTrie newVisited r c newWord depth (level + 1) rows cols) 
                                    [(row+1, col), (row, col+1), (row-1, col), (row, col-1)]
                        else concatMap (\(r, c) -> searchFromCell board newTrie newVisited r c newWord depth (level + 1) rows cols)
                                    [(row+1, col), (row, col+1), (row-1, col), (row, col-1)]
        in foundWords ++ nextWords  
  where
    curr = board !! row !! col