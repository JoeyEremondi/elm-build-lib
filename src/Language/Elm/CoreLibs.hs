 {-# LANGUAGE TemplateHaskell #-}
 module Language.Elm.CoreLibs where
   
import Data.FileEmbed
import Language.Elm.BuildUtil

import Elm.Compiler
import Elm.Compiler.Module

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Monad

import Data.ByteString.Char8 (unpack)

import Data.Maybe (fromJust)

import qualified Language.Elm.CoreSources as Sources

header :: String
header = Sources.header
  
runtime = (fromJust $ nameFromString "Elm.Native.Runtime", Sources.runtime)

nativeDict = Map.fromList nativePairs
  where 
    nativePairs = map (\(n,t) -> (fromJust $ nameFromString n, t)) nativeTextPairs
    nativeTextPairs = Sources.nativeSources

sources = Sources.stdlibSources


--Dependencies standard lib modules have with each other
internalDepsPairs = case (mapM uniqueDeps sources) of
    Left s -> error $ "Failed parsing stdlib:" ++ s
    Right pairs ->  pairs

internalDeps :: Dependencies
internalDeps = Map.fromList internalDepsPairs

stdLib :: CompileResult
stdLib  = case eitherLib of
  Left s -> error $ "Failed building standard library: " ++ s
  Right dict -> dict
  where 
      sourcesDict = Map.fromList $ zip (map fst internalDepsPairs) sources
      eitherLib = compileAll internalDeps "elm-lang" "core" (Map.empty, Map.empty) sourcesDict
     --jsSources = 

--Given a dependency list, return the compile result of the corresponding stdlib
stdLibForSources :: Dependencies -> [String] -> Either String CompileResult
stdLibForSources deps modules = do
    let libDeps = Set.filter (importNotNative) $ findStdLibDeps deps
    let (allJs, allIfaces) = stdLib
    let js = Map.filterWithKey (\d _ -> Set.member d libDeps ) allJs
    let ifaces = Map.filterWithKey (\d _ -> Set.member d libDeps ) allIfaces
    return (js, ifaces)
     
moduleStdlibDeps :: String -> Either String [Name]
moduleStdlibDeps s = do
  (name, deps) <- parseDependencies s
  let stdlibDeps = filter (\d -> Map.member d (fst stdLib)) deps
  return $ List.nub stdlibDeps

    

findStdLibDeps :: Dependencies -> Set.Set Name
findStdLibDeps deps = 
    let
        depSets = Map.elems deps
        topWithDefaults = Set.unions $  [Set.fromList defaultImports] ++ depSets
    --Get dependencies of dependencies
    in traverseDeps internalDeps topWithDefaults

nativesForSources :: Dependencies -> Either String (Map.Map Name String)
nativesForSources deps = do
    let libDeps = findStdLibDeps deps
    let nativeNames = Set.filter (not . importNotNative ) libDeps
    return $ Map.filterWithKey (\d _ -> Set.member d nativeNames) nativeDict