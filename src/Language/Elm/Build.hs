module Language.Elm.Build (compileAll) where

import qualified Data.Map as Map

import Elm.Compiler
import Elm.Compiler.Module
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad (filterM)
import Data.Maybe (fromJust)


elmModuleName :: String -> Either String Elm.Compiler.Module.Name
elmModuleName modul = do
    (name, _) <- parseDependencies modul
    return name



--Reorder the dependencies so that we can compile them in order
resolveDependencies :: [String] -> Either String [Name]
resolveDependencies deps = do
    edgePairs <- mapM parseDependencies deps
    let names = map fst edgePairs
    let edgeMap = Map.fromList edgePairs
    --Get predecessors of each node, needed for the top sort

    let eitherLookUp :: Name -> Map.Map Name [Name] -> Either String [Name] 
        eitherLookUp name map = case Map.lookup name map of
                Nothing -> Left $  "Dependency not found: " ++ nameToString name
                Just b -> Right b
    let n1 `hasEdgeFrom` n2 = case Map.lookup n2 edgeMap of
                Nothing -> False
                Just l -> n1 `elem` l

    let predMap = Map.fromList $ zip names $ map (\end -> [start | start <- names, end `hasEdgeFrom` start ]) names
    let sources = filter (null . snd) edgePairs
    let initialVisitedNodes = Set.fromList $ map fst sources
    let initialOpenList = map fst sources
    let initialResult = map fst sources
    
    
    let seenAllPred :: Set.Set Name -> Name -> Either String Bool
        seenAllPred visited name = do
                predList <- eitherLookUp name predMap
                let unseenPreds = filter (not . (flip Set.member $ visited)) predList
                return $ null unseenPreds
    let topSort _ [] result = return result
        topSort visited (current:otherNodes) resultSoFar = do
                currentEdges <- eitherLookUp current edgeMap
                let alreadySeen = filter (flip Set.member $ visited) currentEdges
                case alreadySeen of
                    [] -> Left "Error: you have a dependency cycle. Your program cannot be compiled"
                    _ -> do
                        let newVisited = Set.insert current visited
                        newSources <- filterM (seenAllPred newVisited) currentEdges
                        topSort newVisited (newSources ++ otherNodes) (resultSoFar ++ newSources)

    topSort initialVisitedNodes initialOpenList initialResult
                        
        
compileAll :: [String] -> Either String String
compileAll modules = do
    nameDeps <- mapM parseDependencies modules
    let names = map fst nameDeps
    let nameDict = Map.fromList $ zip names modules
    orderedNames <- resolveDependencies modules
    let orderedSources = map (fromJust . (flip Map.lookup $ nameDict)) orderedNames
    compileInOrder orderedSources


compileInOrder :: [String] -> Either String String
compileInOrder modules = helper modules Map.empty ""
    where
      helper [] _ src = return src
      helper (modul:otherModules) compiledModules _ = do
          name <- elmModuleName modul
          (newModule, src) <- compile modul compiledModules
          let newCompiled = Map.insert name newModule compiledModules
          helper otherModules newCompiled src   
          
    