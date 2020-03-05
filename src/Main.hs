-- Project: FLP - DKA-2-MKA
-- Author: Ondřej Pavela - xpavel34
-- Year: 2020

module Main where

import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Char
import Text.Printf

import Utils
import Parser
import qualified FiniteAutomaton as FA

-- Convenience type, represents mode of output
-- requested by the user via parameter
data OutputMode = ParsedDFA | MinimizedDFA

parseMode :: String -> Maybe OutputMode
parseMode arg = case arg of
    "-i" -> Just ParsedDFA
    "-t" -> Just MinimizedDFA
    otherwise -> Nothing

main :: IO ()
main = do
    args <- getArgs
    case length args of 
        x | x < 1 -> exit (ArgError MissingArg) True
        x | x > 2 -> exit (ArgError TooManyArgs) True
        x -> return ()

    let (outputMode, reader) = case args of
            [mode] -> (parseMode mode, getContents)
            [mode, path] -> (parseMode mode, catch (readFile path) readExnHandler)

    when (isNothing outputMode) $ exit (ArgError InvalidArgument) True

    contents <- reader
    let fileLines = filter (not . all isSpace) (lines contents)
    dfa <- parseInputFA fileLines

    case fromMaybe ParsedDFA outputMode of
        ParsedDFA -> 
            --print dfa
            FA.outputDFA dfa
            
        MinimizedDFA -> do
            --printf "Parsed: %s\n" (show dfa)
            let cleanDFA = FA.eliminateUnreachableStates dfa
            --printf "Cleaned: %s\n" (show cleanDFA)
            let completeDFA = FA.addSinkState cleanDFA
            --printf "Complete: %s\n" (show completeDFA)
            let minimalDFA = FA.minimizeDFA completeDFA
            --printf "Minimal: %s\n" (show minimalDFA)
            FA.outputDFA minimalDFA
