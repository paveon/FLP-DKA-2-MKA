-- Project: FLP - DKA-2-MKA
-- Author: Ondřej Pavela - xpavel34
-- Year: 2020

module FiniteAutomaton(
    StateID,
    StateSet,
    Alphabet,
    Transition,
    StateTransitions,
    TransitionMap,
    FiniteAutomaton(..),
    eliminateUnreachableStates,
    addSinkState,
    minimizeDFA,
    outputDFA) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Monad
import Text.Printf

import Utils

type StateID = Int
type StateSet = Set.Set StateID
type Alphabet = Set.Set Char
type Transition = (StateID, Char, StateID) -- Single transition

-- Represents all transitions of a single source state
type StateTransitions = Map.Map Char StateID

-- Complete representation of all transitions. All
-- states are mapped to its StateTransitions data structure
type TransitionMap = Map.Map StateID StateTransitions

-- Maps state to its equivalence class, used during minimization
type EqClassMap = Map.Map StateID StateSet

-- Data structure for finite automatons.
-- Suitable only for deterministic FAs
data FiniteAutomaton = FiniteAutomaton {
    states :: StateSet,
    alphabet :: Alphabet,
    initialState :: StateID,
    finalStates :: StateSet,
    transitions :: TransitionMap
}

-- Pretty printing for FAs
instance Show FiniteAutomaton where
  show (FiniteAutomaton s a i f t) = 
      printf "DFA = (S, A, I, F, T)\n\
      \States: %s\n\
      \Alphabet: [%s]\n\
      \Initial State: %d\n\
      \Final States: %s\n\
      \Transitions:\n%s"
      (show $ sort $ Set.toList s) (sort $ Set.toList a)
      i (show $ sort $ Set.toList f) (showTransitionMap t)

-- Pretty printing of TransitionMap data structure
showTransitionMap :: TransitionMap -> String
showTransitionMap transitionMap = if Map.null transitionMap then ""
    else concatMap (stateTransitionsToStr . second Map.toList) (Map.toList transitionMap) where
                tranToStr :: String -> (Char, StateID) -> String
                tranToStr padding (symbol, dstState) =
                    printf "  %s -> %c -> %d\n" padding symbol dstState

                stateTransitionsToStr :: (StateID, [(Char, StateID)]) -> String
                stateTransitionsToStr (src, stateTransitions) = do
                    let srcStr = show src
                    let paddingLength = length srcStr
                    let padding = concat $ replicate paddingLength " "
                    let acc = concatMap (tranToStr padding) stateTransitions
                    printf "  %s%s" srcStr (drop (paddingLength + 2) acc)

-- Converts TransitionMap data structure into a list
-- of individual transitions.
transitionMapToList :: TransitionMap -> [Transition]
transitionMapToList transitionMap = do 
    let tList = map (second Map.toList) $ Map.toList transitionMap
    concatMap (\(src, stateTransitions) -> map (extendPairL src) stateTransitions) tList

-- Creates a map which associates each state to its equivalence class
createEqClassMap :: Set.Set StateSet -> EqClassMap
createEqClassMap = Set.foldl lambda Map.empty where
    lambda :: EqClassMap -> StateSet -> EqClassMap
    lambda map stateSet = Map.union map $
        Map.fromList $ zip (Set.toList stateSet) $ repeat stateSet

-- Outputs DFA in a format suitable for automatic tests
outputDFA :: FiniteAutomaton -> IO ()
outputDFA (FiniteAutomaton s a i f t) = do
    let transitions = transitionMapToList t
    printf "%s\n%s\n%d\n%s\n" 
        (listToStr "," $ Set.toList s)
        (sort $ Set.toList a)
        i 
        (listToStr "," $ Set.toList f)
        
    forM_ transitions (\(src, symbol, dst) -> printf "%d,%c,%d\n" src symbol dst)

-- Eliminates all unreachable states and related transitions from DFA
eliminateUnreachableStates :: FiniteAutomaton -> FiniteAutomaton
eliminateUnreachableStates fa = do
    let transitionMap = transitions fa

    -- Add all states which are directly reachable from the specified
    -- state to the accumulator set
    let addDirectlyReachable :: StateSet -> StateID -> StateSet
        addDirectlyReachable acc state = do
            let stateReachable = fromMaybe Map.empty $ Map.lookup state transitionMap
            Set.union acc (Set.fromList $ Map.elems stateReachable)

    -- Calculate reachability fixpoint for the initial state set
    let expand :: Set.Set StateID -> Set.Set StateID
        expand reachable = Set.foldl addDirectlyReachable reachable reachable

    let isFixedPoint :: Set.Set StateID -> Bool        
        isFixedPoint set = expand set == set

    let initialSet = Set.singleton (initialState fa)
    let reachable = until isFixedPoint expand initialSet
    let unreachable = Set.difference (states fa) reachable

    -- Eliminate all transitions from the unreachable states
    let filteredTransitions = Map.filterWithKey predicate transitionMap where 
            predicate :: StateID -> StateTransitions -> Bool
            predicate state _ = state `notElem` unreachable
    
    fa { states = reachable, transitions = filteredTransitions }

-- Adds sink state to the input DFA. Does nothing if the input DFA is complete
addSinkState :: FiniteAutomaton -> FiniteAutomaton
addSinkState dfa = do
    let sinkID = Set.findMax (states dfa) + 1
    let complementMap = Map.fromList $ zip (Set.toList $ alphabet dfa) (repeat sinkID)
    dfa { transitions = Map.map (`Map.union` complementMap) (transitions dfa) }

-- Determines if both input states belong to the same equivalence class
stateEquivalence :: StateID -> StateID -> FiniteAutomaton -> EqClassMap -> Bool
stateEquivalence s1 s2 fa eqClassMap = all equalDstClass (alphabet fa) where
    -- Retrieve state transition maps for both states
    s1Map = Map.findWithDefault Map.empty s1 (transitions fa)
    s2Map = Map.findWithDefault Map.empty s2 (transitions fa)

    -- Find respective equivalence classes for both states and compare them
    equalDstClass :: Char -> Bool
    equalDstClass symbol = case (Map.lookup symbol s1Map, Map.lookup symbol s2Map) of
        (Just dstS1, Just dstS2) -> Map.lookup dstS1 eqClassMap == Map.lookup dstS2 eqClassMap
        (Nothing, Nothing) -> True
        otherwise -> False

-- Assigns an input state to its equivalence class or creates new equivalence
-- class if appropriate equivalence class does not exist yet.
classifyState :: EqClassMap -> FiniteAutomaton -> [StateSet] -> StateID -> [StateSet]
classifyState eqClassMap fa classes state = do
    let defaultValue = Set.singleton state
    let updateFunction = Set.insert state
    findAndMap (\set -> stateEquivalence state (Set.elemAt 0 set) fa eqClassMap)
        updateFunction defaultValue classes

-- Attempts to partition single equivalence class into multiple smaller classes
-- based on existing transitions and current equivalence class mapping of all states
partitionEqClass :: EqClassMap -> FiniteAutomaton -> StateSet -> Set.Set StateSet
partitionEqClass eqClassMap fa set = 
    Set.fromList $ Set.foldl (classifyState eqClassMap fa) [] set

-- Minimizes complete DFA
minimizeDFA :: FiniteAutomaton -> FiniteAutomaton
minimizeDFA dfa = do
    let transitionMap = transitions dfa
    let initialEqClasses = pairToSet $ Set.partition (`elem` finalStates dfa) (states dfa)

    -- Recursively partitions all equivalence classes until we obtain a fixpoint
    let partitionClasses :: Set.Set StateSet -> (Set.Set StateSet, EqClassMap)
        partitionClasses sets = do
            let eqClassMap = createEqClassMap sets
            let refined = Set.foldl partition Set.empty sets where
                    partition :: Set.Set StateSet -> StateSet -> Set.Set StateSet
                    partition acc set = Set.union acc $ partitionEqClass eqClassMap dfa set

            if refined == sets
                then (sets, eqClassMap)
                else partitionClasses refined

    let (eqClasses, eqClassMap) = partitionClasses initialEqClasses
    let nameMap = Map.fromList $ zip (Set.toList eqClasses) [0..]

    let stateIdToClassId :: StateID -> StateID
        stateIdToClassId stateID = do
            let stateClass = fromMaybe Set.empty $ Map.lookup stateID eqClassMap
            fromMaybe 0 $ Map.lookup stateClass nameMap
    
    -- delta([p], a) = [q] <==> delta(p, a) = q
    let toClassTransition :: (StateID, StateTransitions) -> (StateID, StateTransitions)
        toClassTransition (srcState, stateTransitions) = do
            let classTransitions = Map.map stateIdToClassId stateTransitions
            (stateIdToClassId srcState, classTransitions)
                
    dfa {
        states = Set.fromList $ Map.elems nameMap,
        initialState = stateIdToClassId $ initialState dfa,
        finalStates = Set.map stateIdToClassId (finalStates dfa),
        transitions = Map.fromList $ map toClassTransition $ Map.assocs transitionMap
    }