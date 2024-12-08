module Main (main) where

import qualified SequentialSearch
import qualified ParallelWordsSearch
import InputParser 
import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename, solution] -> do
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
                                        _           -> error "Unknown solution type"

                            results `deepseq` return () -- Force evaluation of cases above, otherwise timer is 0s because of lazy evaluation
                            mapM_ putStrLn results
                            end <- getCurrentTime -- Put after mapM_, solves the laziness issue
                            putStrLn $ "Time taken: " ++ show (diffUTCTime end start)
                
                -- Case when input does not have exactly two lines
                _ -> putStrLn "Error: Input file must contain exactly two lines"
        
        -- Case when there are not exactly two arguments
        _ -> putStrLn "Usage: ./wordsearch <filename> <solution>"
