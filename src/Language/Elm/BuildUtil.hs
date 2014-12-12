{-# LANGUAGE TemplateHaskell #-}
module Language.Elm.BuildUtil where

import qualified Data.Map as Map

import Elm.Compiler
import  Elm.Compiler.Module
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad (filterM)
import Data.Maybe (fromJust)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote 

import qualified Data.List as List
import qualified Data.Set as Set

--Dict of JS and Interfaces that have already been compiled
type CompileResult = (Map.Map Name String, Map.Map Name Interface)

type Dependencies = Map.Map Name (Set.Set Name)


uniqueDeps :: String -> Either String (Name, Set.Set Name)
uniqueDeps s = do
  (name, deps) <- parseDependencies s
  return (name, Set.fromList deps)

elmModuleName :: String -> Either String Elm.Compiler.Module.Name
elmModuleName modul = do
    (name, _) <- uniqueDeps modul
    return name

importNotNative :: Name -> Bool
importNotNative name = if (List.isPrefixOf "Native." nameString)
  then False
  else True 
  where nameString = nameToString name  

stringPairs pairs = map (\(n, nList) -> (nameToString n, map nameToString nList)) pairs

--Keep adding dependencies of dependencies until we're done
traverseDeps :: Dependencies -> Set.Set Name ->  Set.Set Name
traverseDeps depMap startDeps = let
    notNativeStartDeps = Set.filter (importNotNative) startDeps
    nextLevelDeps = Set.unions $ map (\libName ->  depMap Map.! libName) (Set.toList notNativeStartDeps)
    uniqueDeps = Set.union startDeps nextLevelDeps
  in 
    if (Set.size uniqueDeps == Set.size startDeps)
    then uniqueDeps
    else traverseDeps depMap uniqueDeps
  
--Reorder the dependencies so that we can compile them in order
--Already have: bool indicating whether a dependency is already in our list of 
resolveDependencies :: (Name -> Bool) -> Dependencies -> [Name] -> Either String [Name]
resolveDependencies alreadyHave edgeMap names = 
    case eitherDepList of
        Right deps -> Right deps
        Left s -> Left $ "Error resolving dependencies:\n" ++ s
      where 
        eitherDepList :: Either String [Name]
        eitherDepList = do
            let notNativeOrCompiled n = importNotNative n && (not $ alreadyHave n)
            --edgePairs <- mapM uniqueDeps deps
            --let names = map fst edgePairs
            --let edgeMap = Map.fromList edgePairs
            --Get predecessors of each node, needed for the top sort

            let eitherLookUp :: Name -> Map.Map Name (Set.Set Name) -> Either String (Set.Set Name)
                eitherLookUp name map = case (notNativeOrCompiled name, Map.lookup name map) of
                        (False, _) ->  Right (Set.empty) --Native modules have no dependencies, are always sinks
                        (True, Nothing) -> Left $  "Dependency not found: " ++ nameToString name
                        (True, Just b) -> Right b
            let edgeFromTo n1 n2 = case Map.lookup n1 edgeMap of
                        Nothing -> False
                        Just set -> Set.member n2 set

            let predList = zip names $ map (\end -> Set.fromList [start | start <- names, edgeFromTo start end]) names
            let predMap = Map.fromList predList
            let sources = filter (Set.null . snd) predList
            let initialVisitedNodes =  Set.empty
            let initialOpenList = map fst sources
            let initialResult = map fst sources
            
            
            let seenAllPred :: Set.Set Name -> Name -> Either String Bool
                seenAllPred visited name = do
                        currentPreds <- eitherLookUp name predMap
                        let unseenPreds = Set.filter (not . (flip Set.member $ visited)) currentPreds
                        return  $ Set.null unseenPreds
            let topSort :: (Set.Set Name) -> [Name] -> [Name] -> Either String [Name]
                topSort _ [] result = return result
                topSort visited (current:otherNodes) resultSoFar = do
                        currentEdgesAndNatives <- eitherLookUp current edgeMap
                        let currentEdges = filter notNativeOrCompiled $ Set.toList currentEdgesAndNatives
                        let alreadySeen = filter (flip Set.member $ visited) currentEdges
                        case ( alreadySeen) of
                            (h:_) -> Left $ "Error: you have a dependency cycle. Your program cannot be compiled. "
                               ++ (nameToString current) ++ " to " ++ (nameToString h)
                            _ -> do
                                let newVisited = Set.insert current visited
                                newSources <- filterM (seenAllPred newVisited) currentEdges
                                topSort newVisited (newSources ++ otherNodes) (newSources ++ resultSoFar ) --Build in reverse order

            topSort initialVisitedNodes (initialOpenList) ( initialResult)
                        
        
compileAll :: Dependencies -> String -> String -> CompileResult -> [String] -> Either String CompileResult
compileAll deps user packageName (startJS, startIfaces) modules = do
    --nameDeps <- mapM uniqueDeps modules
    let names = Map.keys deps
    --let allNames = traverseDeps deps names
    let nameDict = Map.fromList $ zip names modules
    orderedNames <- resolveDependencies (\n -> Map.member n startIfaces) deps names
    let orderedSources = map (fromJust . (flip Map.lookup $ nameDict)) orderedNames
    compileInOrder user packageName (startJS, startIfaces) orderedSources
    





compileInOrder :: String -> String -> CompileResult -> [String] -> Either String CompileResult
compileInOrder user packageName alreadyCompiled modules = helper modules alreadyCompiled
    where
      helper [] ret = return ret
      helper (modul:otherModules) (compiledJS, compiledIfaces) = do
          name <- elmModuleName modul
          (newModule, src) <- compileWithName name user packageName modul compiledIfaces
          let newSources = Map.insert name src compiledJS
          let newIfaces = Map.insert name newModule compiledIfaces
          helper otherModules (newSources, newIfaces)
          
--Helper for better error messages when compiling
compileWithName name user packageName modul compiledModules = case (compile user packageName modul compiledModules) of
  Left s -> Left $ "Error compiling module " ++ (nameToString name) ++ "\n" ++ s
  Right ret -> Right ret