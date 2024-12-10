module SequentialSearch2 (findWords, insertWord, Trie(..), emptyTrie, searchFromCell) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Parallel (par, pseq)
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

findWords :: [[Char]] -> [String] -> [String]
findWords board targetWords = 
    nub $ concat $ parMap rseq (\(r, c) -> searchFromCell board trie Set.empty r c [])  
    [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
  where
    rows = length board
    cols = length (head board)
    trie = foldr insertWord emptyTrie targetWords  

-- Search from a specific cell
searchFromCell :: [[Char]] -> Trie -> Set (Int, Int) -> Int -> Int -> String -> [String]
searchFromCell board trie visited row col currWord
    | row < 0 || row >= rows || col < 0 || col >= cols = [] 
    | Set.member (row, col) visited = []  
    | not (Map.member curr (children trie)) = []  
    | otherwise = 
        let newTrie = fromMaybe emptyTrie (Map.lookup curr (children trie))  
            newWord = currWord ++ [curr]  
            foundWords = [newWord | isEnd newTrie]  
            newVisited = Set.insert (row, col) visited  
            nextWords = searchNextWords board newTrie newVisited (row, col) newWord
        in foundWords ++ nextWords  
  where
    rows = length board
    cols = length (head board)
    curr = board !! row !! col

searchNextWords :: [[Char]] -> Trie -> Set (Int, Int) -> (Int, Int) -> String -> [String]
searchNextWords board trie visited (row, col) currWord = 
    nf1 `par` nf2 `par` nf3 `par` nf4 `pseq`
    nf1 ++ nf2 ++ nf3 ++ nf4  
  where
    nf1 = searchFromCell board trie visited (row+1) col currWord
    nf2 = searchFromCell board trie visited row (col+1) currWord
    nf3 = searchFromCell board trie visited (row-1) col currWord
    nf4 = searchFromCell board trie visited row (col-1) currWord
