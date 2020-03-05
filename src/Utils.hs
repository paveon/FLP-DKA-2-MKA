-- Project: FLP - DKA-2-MKA
-- Author: Ondřej Pavela - xpavel34
-- Year: 2020

module Utils where

import System.Exit
import System.IO
import Control.Exception
import Control.Monad
import Data.List
import qualified Data.Set as Set

listToStr :: (Show a, Ord a) => String -> [a] -> String
listToStr delim list = intercalate delim . map show $ sort list

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x, y]

pairToSet :: Ord a => (a, a) -> Set.Set a
pairToSet (x,y) = Set.fromList [x, y]

extendPairR :: (a, b) -> c -> (a, b, c)
extendPairR (x, y) z = (x, y, z)

extendPairL :: c -> (a, b) -> (c, a, b)
extendPairL z (x, y) = (z, x, y)

-- Finds single element in a list based on predicate, transforms the value
-- and returns updated list. Appends default value at the end of the list
-- if the searched value is not present in the list.
findAndMap :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
findAndMap _ _ defVal [] = [defVal]
findAndMap p f defVal (x : xs) = if p x then f x : xs else x : findAndMap p f defVal xs

-- Split string based on delimiter character
-- and distinguish between cases such as "1,2,3" and "1,2,3,"
splitString :: Char -> String -> [String]
splitString delimiter string = case dropWhile (== delimiter) string of
    "" -> [""]
    remainder ->
        case elemIndex ',' remainder of
            Just x -> word : splitString delimiter stringTail where 
                (word, stringTail) = splitAt x remainder
            Nothing -> [remainder]

readExnHandler :: IOException -> IO a
readExnHandler err = do
    hPutStrLn stderr ("[Exception] " ++ show err)
    exitFailure

data Error = 
    ParserError ParserError
    | ArgError ArgError

data ArgError = 
    MissingArg 
    | TooManyArgs 
    | InvalidArgument

data ParserError =  
    InvalidFormat
    | InvalidInitialState
    | InvalidFinalStates
    | InvalidTransChar
    | InvalidTransState
    | NondeterministicFA
    | EmptyStateSet

instance Show Error where
    show (ArgError e) = show e
    show (ParserError e) = show e

instance Show ArgError where
    show MissingArg = "Missing argument!"
    show TooManyArgs = "Too many arguments!"
    show InvalidArgument = "Invalid argument value!"

instance Show ParserError where
    show InvalidFormat = "Invalid input file format!"
    show InvalidInitialState = "Specified initial state does not exist!"
    show InvalidFinalStates = "Specified set of final states is not a subset of all states!"
    show InvalidTransChar = "Transition Char is not contained in specified alphabet!"
    show InvalidTransState = "Transition state does not exist!"
    show NondeterministicFA = "Specified FA is not deterministic!"
    show EmptyStateSet = "Input FA has empty state set!"

exit :: Error -> Bool -> IO a
exit errorType outputHelp = do
    let helpString = "Usage: dka-2-mka (-i | -t) [inputFile]\n\
    \    -i            output internal representation of parsed DFA\n\
    \    -t            output minimized input DFA\n\
    \    [inputFile]   input file containing valid DFA. DFA is read\n\
    \                  from standard input if omitted\n"
    hPutStrLn stderr ("[Error] " ++ show errorType)
    when outputHelp $ putStrLn helpString
    exitFailure