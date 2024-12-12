module Main (main) where

import qualified SequentialSearch
import qualified ParallelDepthSearch
import qualified ParallelWordsSearch
import qualified ParallelSubgridSearch
import InputParser 
import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename, solution] -> processFile filename solution Nothing
        [filename, solution, paramStr] ->
            case reads paramStr :: [(Int, String)] of
                [(n, "")] | n > 0 -> processFile filename solution (Just n)
                _ -> error "Invalid input for number of subgrid / depth: please provide a positive integer."
        _ -> putStrLn "Usage: ./wordsearch <filename> <solution> <optional: number of subgrid / depth>"

processFile :: FilePath -> String -> Maybe Int -> IO ()
processFile filename solution param = do
    contents <- readFile filename
    case lines contents of
        [boardStr, wordsStr] -> do
            let board = parseBoard boardStr
            let wordsList = parseWords wordsStr
            
            -- Debug output
            putStrLn "Parsed Board:"
            mapM_ print board
            putStrLn "Parsed Words:"
            print wordsList
            
            if null board || any null board
                then putStrLn "Error: Invalid board format"
                else do
                    -- Time the findWords operation
                    start <- getCurrentTime
                    let results = runSolution solution board wordsList param
                    results `deepseq` return () -- Force evaluation
                    mapM_ putStrLn results
                    end <- getCurrentTime
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)
        _ -> putStrLn "Error: Input file must contain exactly two lines"

runSolution :: String -> [[Char]] -> [String] -> Maybe Int -> [String]
runSolution solution board wordsList param =
    case solution of
        "sequential" -> SequentialSearch.findWords board wordsList
        "parallelwords" -> ParallelWordsSearch.findWords board wordsList
        "paralleldepth" ->
            case param of
                Just n -> ParallelDepthSearch.findWords n board wordsList
                Nothing -> error "Missing depth argument for 'paralleldepth' solution."
        "parallelsubgrids" ->
            case param of
                Just n -> ParallelSubgridSearch.findWordsSubgrids n board wordsList
                Nothing -> error "Missing subgrids argument for 'parallelsubgrids' solution."
        _ -> error "Invalid solution argument."