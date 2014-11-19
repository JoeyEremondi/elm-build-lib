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

--TODO remove
import Debug.Trace (trace)

uniqueDeps :: String -> Either String (Name, [Name])
uniqueDeps s = do
  (name, deps) <- parseDependencies s
  return (name, List.nub deps)

elmModuleName :: String -> Either String Elm.Compiler.Module.Name
elmModuleName modul = do
    (name, _) <- uniqueDeps modul
    return name

importNotNative :: Name -> Bool
importNotNative name = if (List.isPrefixOf "Native." nameString)
  then False
  else True 
  where nameString = nameToString name  



  
--Reorder the dependencies so that we can compile them in order
resolveDependencies :: [String] -> Either String [Name]
resolveDependencies deps = do
    edgePairs <- mapM uniqueDeps deps
    let names = map fst edgePairs
    let edgeMap = trace ("Got dependency map " ++ show ((map (\(nam, edges) -> "Name " ++ (nameToString nam) ++ " Edges " ++ (show $ map nameToString edges ))) edgePairs ) ) $ Map.fromList edgePairs
    --Get predecessors of each node, needed for the top sort

    let eitherLookUp :: Name -> Map.Map Name [Name] -> Either String [Name] 
        eitherLookUp name map = case (importNotNative name, Map.lookup name map) of
                (False, _) ->  Right [] --Native modules have no dependencies, are always sinks
                (True, Nothing) -> Left $  "Dependency not found: " ++ nameToString name
                (True, Just b) -> Right b
    let n1 `hasEdgeFrom` n2 = case Map.lookup n2 edgeMap of
                Nothing -> False
                Just l -> n1 `elem` l

    let predList = zip names $ map (\end -> [start | start <- names, end `hasEdgeFrom` start ]) names
    let predMap = Map.fromList predList
    let sources = filter (null . (filter importNotNative) . snd) predList
    let initialVisitedNodes =  Set.fromList $ map fst sources
    let initialOpenList = map fst sources
    let initialResult = map fst sources
    
    
    let seenAllPred :: Set.Set Name -> Name -> Either String Bool
        seenAllPred visited name = do
                predList <- eitherLookUp name predMap
                let unseenPreds = filter (not . (flip Set.member $ visited)) predList
                return $ null unseenPreds
    let topSort _ [] result = return result
        topSort visited (current:otherNodes) resultSoFar = do
                currentEdgesAndNatives <- eitherLookUp current edgeMap
                let currentEdges = trace ("All edges edges " ++ show (map nameToString currentEdgesAndNatives)) $ filter importNotNative currentEdgesAndNatives
                let alreadySeen = trace ("Current edges " ++ show (map nameToString currentEdges)) $ filter (flip Set.member $ visited) currentEdges
                case alreadySeen of
                    (h:_) -> Left $ "Error: you have a dependency cycle. Your program cannot be compiled. "
                       ++ (nameToString current) ++ " to " ++ (nameToString h)
                    _ -> do
                        let newVisited = Set.insert current visited
                        newSources <- filterM (seenAllPred newVisited) currentEdges
                        trace ("In topsort " ++ (nameToString current) ++ "  " ++ (show $ map nameToString otherNodes) ++ " edges " ++ (show $ map nameToString currentEdges)) $ topSort newVisited (newSources ++ otherNodes) (resultSoFar ++ newSources)

    trace ("Initial sources " ++ show (map ((map nameToString) . snd) sources)) $ topSort initialVisitedNodes initialOpenList initialResult
                        
        
compileAll :: String -> String -> (Map.Map Name Interface) -> [String] -> Either String (String, Map.Map Name Interface)
compileAll user packageName startIfaces modules = do
    nameDeps <- mapM uniqueDeps modules
    let names = map fst nameDeps
    let nameDict = Map.fromList $ zip names modules
    orderedNames <- resolveDependencies modules
    let orderedSources =  trace("Got ordered names: " ++ (show $ map nameToString orderedNames) ) $ map (fromJust . (flip Map.lookup $ nameDict)) orderedNames
    compileInOrder user packageName startIfaces orderedSources





compileInOrder :: String -> String -> (Map.Map Name Interface) -> [String] -> Either String (String, Map.Map Name Interface)
compileInOrder user packageName startIfaces modules = helper modules startIfaces ""
    where
      helper [] ifaces src = return (src, ifaces)
      helper (modul:otherModules) compiledModules _ = do
          name <- elmModuleName modul
          (newModule, src) <- compile user packageName modul compiledModules
          let newCompiled = Map.insert name newModule compiledModules
          helper otherModules newCompiled src   