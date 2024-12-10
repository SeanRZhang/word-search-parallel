module Main (main) where

import qualified SequentialSearch
import qualified SequentialSearch2
import qualified ParallelWordsSearch
import qualified ParallelWordsSearch2
import InputParser 
import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename, solution, subgridsStr] -> do
            -- Parse subgrids argument
            let subgrids = case reads subgridsStr :: [(Int, String)] of
                    [(n, "")] | n > 0 -> n
                    _ -> error "Invalid input for subgrids: please provide a positive integer."
            
            -- Read the input file contents
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
                            -- Time the findWords operation based on the solution argument
                            start <- getCurrentTime
                            let results = 
                                    case solution of
                                        "sequential" -> SequentialSearch.findWords board wordsList
                                        "parallelwords" -> ParallelWordsSearch.findWords board wordsList
                                        "parallelwords2" -> ParallelWordsSearch2.findWordsParallel subgrids board wordsList
                                        _ -> error "Invalid solution argument"
                            results `deepseq` return () -- Force evaluation of cases above, otherwise timer is 0s due to lazy evaluation
                            mapM_ putStrLn results
                            end <- getCurrentTime -- Put after mapM_, solves the laziness issue
                            putStrLn $ "Time taken: " ++ show (diffUTCTime end start)
                
                -- Case when input does not have exactly two lines
                _ -> putStrLn "Error: Input file must contain exactly two lines"
        
        -- Case when there are not exactly three arguments
        [filename, solution] -> do
            -- Read the input file
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
                            -- Time the findWords operation based on the solution argument
                            start <- getCurrentTime
                            let results = 
                                    case solution of
                                        "sequential" -> SequentialSearch.findWords board wordsList
                                        "sequential2" -> SequentialSearch2.findWords board wordsList
                                        "parallelwords" -> ParallelWordsSearch.findWords board wordsList
                                        _ -> error "Invalid solution argument. Note: 'parallelwords2' requires additional subgrids argument."
                            results `seq` return () -- Force evaluation of cases above, otherwise timer is 0s due to lazy evaluation
                            end <- getCurrentTime
                            mapM_ putStrLn results
                            putStrLn $ "Time taken: " ++ show (diffUTCTime end start)
                
                -- Case when input does not have exactly two lines
                _ -> putStrLn "Error: Input file must contain exactly two lines"

        -- Case when arguments are incorrect
        _ -> putStrLn "Usage: ./wordsearch <filename> <solution> <optional: number of subgrids>"
