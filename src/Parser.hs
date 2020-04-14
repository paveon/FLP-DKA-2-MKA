-- Project: FLP - DKA-2-MKA
-- Author: Ondřej Pavela - xpavel34
-- Year: 2020

module Parser (parseInputFA) where

import System.IO
import System.Exit
import Control.Monad
import Data.Maybe
import Data.Char
import Text.Read
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Map as Map

import FiniteAutomaton
import Utils


-- Parses a single line of input into a set of states
-- Expects list of integers separated by commas without any excess comma at the end
parseStates :: String -> IO StateSet
parseStates inputLine = do
    let cleanedLine = filter (not . isSpace) inputLine
    if null cleanedLine then return Set.empty
    else do
        -- Try to parse everything to int and then create a set
        let states = map readMaybe $ splitString ',' inputLine
        when (Nothing `elem` states) $ exit (ParserError InvalidFormat) False
        return (Set.fromList $ catMaybes states)

-- Parses a single line of input into an alphabet which is 
-- represented as a set of characters. Expects string containing only
-- lowercase ASCII letters and whitespace characters
parseAlphabet :: String -> IO Alphabet
parseAlphabet inputLine = do
    let filteredAlphabet = Set.fromList $ filter (not . isSpace) inputLine
    if all isAsciiLower (Set.elems filteredAlphabet)
        then return filteredAlphabet
        else exit (ParserError InvalidFormat) False

-- Parses a string into a single integer representing an state
-- Expects string with single integer and possible whitespaces
parseInitialState :: String -> StateSet -> IO StateID
parseInitialState inputLine existingStates = do
    states <- parseStates inputLine
    when (Set.size states /= 1) $ exit (ParserError InvalidFormat) False
    let initialState = head $ Set.elems states
    if Set.member initialState existingStates
        then return initialState
        else exit (ParserError InvalidInitialState) False

-- Same as previous functions with additional subset check
-- for specified final states
parseFinalStates :: String -> StateSet -> IO StateSet
parseFinalStates inputLine existingStates = do
    finalStates <- parseStates inputLine
    if finalStates `Set.isSubsetOf` existingStates
        then return finalStates
        else exit (ParserError InvalidFinalStates) False

-- Checks if transition symbol specified in "id_1,symbol,id_2" transition
-- consists of a single alphabet character
parseTransitionSymbol :: String -> Alphabet -> IO Char
parseTransitionSymbol symbolStr alphabet = do
    when (length symbolStr /= 1) $ exit (ParserError InvalidFormat) False
    let symbol = head symbolStr
    when (symbol `Set.notMember` alphabet) $ 
        exit (ParserError InvalidTransChar) False

    return symbol

-- Checks if string contains valid existing state ID
parseTransitionState :: String -> StateSet -> IO StateID
parseTransitionState stateSrc existingStates = do
    let srcState = readMaybe stateSrc :: Maybe StateID
    case srcState of
        Nothing -> exit (ParserError InvalidFormat) False
        Just state -> do
            when (state `Set.notMember` existingStates) $ 
                exit (ParserError InvalidTransState) False

            return state

-- Parses input string into a (src,symbol,dst) tuple which
-- represents single transition
parseTransition :: String -> StateSet -> Alphabet -> IO Transition
parseTransition inputLine existingStates alphabet = do
    let splitLine = splitString ',' inputLine
    when (length splitLine /= 3) $ exit (ParserError InvalidFormat) False
    srcState <- parseTransitionState (head splitLine) existingStates
    dstState <- parseTransitionState (splitLine !! 2) existingStates
    symbol <- parseTransitionSymbol (filter (not . isSpace) $ splitLine !! 1) alphabet

    return (srcState, symbol, dstState)

-- Parse all remaining lines into a TransitionMap (src -> (symbol -> dst))
-- structure based on previously parsed set of states and alphabet
parseTransitions :: [String] -> StateSet -> Alphabet -> IO TransitionMap
parseTransitions lines existingStates alphabet =
    foldl createTransition (return Map.empty) lines where
        -- Parse new transition and insert it into the TransitionMap data structure
        createTransition :: IO TransitionMap -> String -> IO TransitionMap
        createTransition transitionMapIO inputLine = do
            (srcState, symbol, dstState) <- parseTransition inputLine existingStates alphabet
            transitionMap <- transitionMapIO

            -- Update transition map for single source state
            let stateTransitions = fromMaybe Map.empty $ Map.lookup srcState transitionMap
            let existingDst = Map.lookup symbol stateTransitions
            stateTransitions' <- case existingDst of
                Nothing -> return $ Map.insert symbol dstState stateTransitions
                Just x -> do
                    -- Transition already exists, ignore duplicates
                    -- and fail on non-determinism
                    when (dstState /= x) $ exit (ParserError NondeterministicFA) False
                    return stateTransitions

            return $ Map.insert srcState stateTransitions' transitionMap

-- Parse input data and create possibly incomplete DFA instance
-- Output DFA might contain unreachable states 
parseInputFA :: [String] -> IO FiniteAutomaton
parseInputFA input = do
    when (length input < 4) $ exit (ParserError InvalidFormat) False
    let (s : a : i : f : t) = input

    states <- parseStates s
    when (Set.null states) $ exit (ParserError InvalidFormat) False

    alphabet <- parseAlphabet a
    initialState <- parseInitialState i states
    finalStates <- parseFinalStates f states

    let transitionLines = filter (not . all isSpace) t
    transitions <- parseTransitions transitionLines states alphabet

    return FiniteAutomaton {
        states = states,
        alphabet = alphabet,
        initialState = initialState,
        finalStates = finalStates,
        transitions = transitions
    }