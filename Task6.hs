module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, mapMaybe)

type Graph n = Map n (Set n)

path :: (Ord node) => Graph node -> node -> node -> Set node -> Maybe [node]
path g start end visited
  | Set.member start visited = Nothing
  | start == end = Just []
  | otherwise
    = do
       let visited' = Set.insert start visited
       nexts <- Map.lookup start g
       listToMaybe
         $ mapMaybe
            (\next -> do
               pathCont <- path g next end visited'
               Just (next:pathCont))
            (Set.toList nexts)

bridge :: (Ord n) => n -> n -> Graph n -> Graph n
bridge u v g = 
    let 
        isPath = case path g u v Set.empty of 
            Nothing -> False
            Just _ -> True
        newGraph = if isPath 
            then g 
            else Map.insertWith Set.union u (Set.singleton v) 
           (Map.insertWith Set.union v Set.empty g)
        in newGraph


disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint u v = if Set.null $ Set.intersection u  v
    then True
    else False

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g n = case Map.lookup n g of 
    Nothing -> False
    Just children -> 
        let 
            visited = Set.singleton n 
            hasDisjointCycle = disjoint visited children
        
        in (not hasDisjointCycle || any (\child -> hasCycleReccursion g child visited) (Set.toList children))

hasCycleReccursion :: (Ord n) => Graph n -> n -> Set n -> Bool
hasCycleReccursion g n visited 
    | Set.member n visited = True
    | otherwise = case Map.lookup n g of
        Nothing -> False
        Just children ->
            let
                newVisited = Set.insert n visited
            in
                any (\child -> hasCycleReccursion g child newVisited) (Set.toList children)
