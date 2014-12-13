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

--The Elm header which we need to append to the top of a linked JS file
header :: String
header = Sources.header

--Pair of the runtime and its name, to be added to a dictionary  
runtime = (fromJust $ nameFromString "Elm.Native.Runtime", Sources.runtime)

--A dictionary mapping names to the JS source code for native modules in the standard library
nativeDict = Map.fromList nativePairs
  where 
    nativePairs = map (\(n,t) -> (fromJust $ nameFromString n, t)) nativeTextPairs
    nativeTextPairs = Sources.nativeSources

--A list of source files for each standard library Elm module
sources = Sources.stdlibSources


--Dependencies standard lib modules have with each other
internalDepsPairs = case (mapM uniqueDeps sources) of
    Left s -> error $ "Failed parsing stdlib:" ++ s
    Right pairs ->  pairs

--The dependency graph of the standard library modules
internalDeps :: Dependencies
internalDeps = Map.fromList internalDepsPairs

--The compiled JavaScript of the entire standard library
stdLib :: CompileResult
stdLib  = case eitherLib of
  Left s -> error $ "Failed building standard library: " ++ s
  Right dict -> dict
  where 
      sourcesDict = Map.fromList $ zip (map fst internalDepsPairs) sources
      eitherLib = compileAll internalDeps "elm-lang" "core" (Map.empty, Map.empty) sourcesDict
     --jsSources = 

--Given a dependency list, return the compile result of the standard library
--Omitting unneeded modules
stdLibForSources :: Dependencies -> [String] -> Either String CompileResult
stdLibForSources deps modules = do
    let libDeps = Set.filter (importNotNative) $ findStdLibDeps deps
    let (allJs, allIfaces) = stdLib
    let js = Map.filterWithKey (\d _ -> Set.member d libDeps ) allJs
    let ifaces = Map.filterWithKey (\d _ -> Set.member d libDeps ) allIfaces
    return (js, ifaces)

--Find the standard library modules dthat a module depends on    
moduleStdlibDeps :: String -> Either String [Name]
moduleStdlibDeps s = do
  (name, deps) <- parseDependencies s
  let stdlibDeps = filter (\d -> Map.member d (fst stdLib)) deps
  return $ List.nub stdlibDeps

--Given a mapping of names to dependencies, expand the dependencies
--To inlcude their dependencies, etc. i.e. transitivity
findStdLibDeps :: Dependencies -> Set.Set Name
findStdLibDeps deps = 
    let
        depSets = Map.elems deps
        topWithDefaults = Set.unions $  [Set.fromList defaultImports] ++ depSets
    --Get dependencies of dependencies
    in traverseDeps internalDeps topWithDefaults

--Given the dependency graph of some sources, find which Native modules it depends on
nativesForSources :: Dependencies -> Either String (Map.Map Name String)
nativesForSources deps = do
    let libDeps = findStdLibDeps deps
    let nativeNames = Set.filter (not . importNotNative ) libDeps
    return $ Map.filterWithKey (\d _ -> Set.member d nativeNames) nativeDict