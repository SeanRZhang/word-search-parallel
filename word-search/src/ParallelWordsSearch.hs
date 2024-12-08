module ParallelWordsSearch (findWords, searchSingleWord, searchFromCell) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Control.Parallel

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

-- Type alias for position
type Pos = (Int, Int)

findWords :: [[Char]] -> [String] -> [String]
findWords board targetWords = 
    let trie = foldr insertWord emptyTrie targetWords
        results = foldl (\acc word -> 
            let newResult = searchSingleWord board trie word
            in newResult `par` (newResult ++ acc)) [] targetWords
    in nub results

searchSingleWord :: [[Char]] -> Trie -> String -> [String]
searchSingleWord board trie word = 
    searchUntilFound Set.empty [(r,c) | r <- [0..rows-1], c <- [0..cols-1]]
  where
    rows = length board
    cols = length (head board)
    
    searchUntilFound _ [] = []
    searchUntilFound visited ((r,c):rest) =
        case searchFromCell board trie r c [] word Set.empty of
            [] -> searchUntilFound visited rest
            found -> found

-- Modified to use Set for visited positions
searchFromCell :: [[Char]] -> Trie -> Int -> Int -> String -> String -> Set.Set Pos -> [String]
searchFromCell board trie row col currWord targetWord visited
    | row < 0 || row >= rows || col < 0 || col >= cols = []
    | Set.member (row, col) visited = []
    | not (Map.member curr (children trie)) = []
    | otherwise = 
        let newTrie = fromMaybe emptyTrie (Map.lookup curr (children trie))
            newWord = currWord ++ [curr]
            foundWords = [newWord | isEnd newTrie && newWord == targetWord]
            newVisited = Set.insert (row, col) visited
            nextWords = concatMap (\(dr,dc) -> 
                searchFromCell board newTrie (row+dr) (col+dc) newWord targetWord newVisited
                ) [(0,1), (1,0), (0,-1), (-1,0)]
        in foundWords ++ nextWords
  where
    rows = length board
    cols = length (head board)
    curr = board !! row !! col