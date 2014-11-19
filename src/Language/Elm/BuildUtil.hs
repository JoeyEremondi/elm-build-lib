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
resolveDependencies :: (Name -> Bool) -> [String] -> Either String [Name]
resolveDependencies alreadyHave deps = 
    case eitherDepList of
        Right deps -> Right deps
        Left s -> Left $ "Error resolving dependencies:\n" ++ s
      where eitherDepList = do
            let notNativeOrCompiled n = importNotNative n && (not $ alreadyHave n)
            edgePairs <- mapM uniqueDeps deps
            let names = map fst edgePairs
            let edgeMap = Map.fromList edgePairs
            --Get predecessors of each node, needed for the top sort

            let eitherLookUp :: Name -> Map.Map Name [Name] -> Either String [Name] 
                eitherLookUp name map = case (notNativeOrCompiled name, Map.lookup name map) of
                        (False, _) ->  Right [] --Native modules have no dependencies, are always sinks
                        (True, Nothing) -> Left $  "Dependency not found: " ++ nameToString name
                        (True, Just b) -> Right b
            let edgeFromTo n1 n2 = case Map.lookup n1 edgeMap of
                        Nothing -> False
                        Just l -> n2 `elem` l

            let predList = zip names $ map (\end -> [start | start <- names, edgeFromTo start end]) names
            let predMap = Map.fromList predList
            let sources = filter (null . snd) predList
            let initialVisitedNodes =  Set.empty
            let initialOpenList = map fst sources
            let initialResult = map fst sources
            
            
            let seenAllPred :: Set.Set Name -> Name -> Either String Bool
                seenAllPred visited name = do
                        currentPreds <- eitherLookUp name predMap
                        let unseenPreds = filter (not . (flip Set.member $ visited)) currentPreds
                        return  $ null unseenPreds
            let topSort _ [] result = return result
                topSort visited (current:otherNodes) resultSoFar = do
                        currentEdgesAndNatives <- eitherLookUp current edgeMap
                        let currentEdges = filter notNativeOrCompiled currentEdgesAndNatives
                        let alreadySeen = filter (flip Set.member $ visited) currentEdges
                        case alreadySeen of
                            (h:_) -> Left $ "Error: you have a dependency cycle. Your program cannot be compiled. "
                               ++ (nameToString current) ++ " to " ++ (nameToString h)
                            _ -> do
                                let newVisited = Set.insert current visited
                                newSources <- filterM (seenAllPred newVisited) currentEdges
                                topSort newVisited (newSources ++ otherNodes) (newSources ++ resultSoFar ) --Build in reverse order

            topSort initialVisitedNodes initialOpenList initialResult
                        
        
compileAll :: String -> String -> (Map.Map Name Interface) -> [String] -> Either String (String, Map.Map Name Interface)
compileAll user packageName startIfaces modules = do
    nameDeps <- mapM uniqueDeps modules
    let names = map fst nameDeps
    let nameDict = Map.fromList $ zip names modules
    orderedNames <- resolveDependencies (\n -> Map.member n startIfaces) modules
    let orderedSources = map (fromJust . (flip Map.lookup $ nameDict)) orderedNames
    compileInOrder user packageName startIfaces orderedSources





compileInOrder :: String -> String -> (Map.Map Name Interface) -> [String] -> Either String (String, Map.Map Name Interface)
compileInOrder user packageName startIfaces modules = helper modules startIfaces ""
    where
      helper [] ifaces src = return (src, ifaces)
      helper (modul:otherModules) compiledModules _ = do
          name <- elmModuleName modul
          (newModule, src) <- compileWithName name user packageName modul compiledModules
          let newCompiled = Map.insert name newModule compiledModules
          helper otherModules newCompiled src   
          
--Helper for better error messages when compiling
compileWithName name user packageName modul compiledModules = case (compile user packageName modul compiledModules) of
  Left s -> Left $ "Error compiling module " ++ (nameToString name) ++ "\n" ++ s
  Right ret -> Right ret