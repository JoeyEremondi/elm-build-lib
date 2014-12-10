 {-# LANGUAGE TemplateHaskell #-}
 module Language.Elm.CoreLibs where
   
import Data.FileEmbed
import Language.Elm.BuildUtil

import Elm.Compiler
import Elm.Compiler.Module

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad

import Data.ByteString.Char8 (unpack)

import Data.Maybe (fromJust)

import qualified Language.Elm.CoreSources as Sources

header :: String
header = Sources.header
  
runtime = (fromJust $ nameFromString "Runtime", Sources.runtime)

nativeDict = Map.fromList nativePairs
  where 
    nativePairs = map (\(n,t) -> (fromJust $ nameFromString n, t)) nativeTextPairs
    nativeTextPairs = Sources.nativeSources

sources = Sources.stdlibSources

--Dependencies standard lib modules have with each other
internalDeps :: Map.Map Name [Name]
internalDeps = case (mapM parseDependencies sources) of
    Left s -> error $ "Failed parsing stdlib:" ++ s
    Right pairs -> Map.fromList pairs

stdLib :: CompileResult
stdLib  = case eitherLib of
  Left s -> error $ "Failed building standard library: " ++ s
  Right dict -> dict
  where 
      eitherLib = compileAll "elm-lang" "core" (Map.empty, Map.empty) sources
     --jsSources = 

--Given a dependency list, return the compile result of the corresponding stdlib
stdLibForSources :: [String] -> Either String CompileResult
stdLibForSources modules = do
    libDeps <- filter (importNotNative) `fmap` stdLibDeps modules
    let (allJs, allIfaces) = stdLib
    let js = Map.filterWithKey (\d _ -> List.elem d libDeps ) allJs
    let ifaces = Map.filterWithKey (\d _ -> List.elem d libDeps ) allIfaces
    return (js, ifaces)
     
moduleStdlibDeps :: String -> Either String [Name]
moduleStdlibDeps s = do
  (name, deps) <- parseDependencies s
  let stdlibDeps = filter (\d -> Map.member d (fst stdLib)) deps
  return $ List.nub stdlibDeps

--Keep adding dependencies of dependencies until we're done
--TODO this is slow and awful. Do a proper DFS
traverseDeps :: [Name] ->  [Name]
traverseDeps startDeps = let
    notNativeStartDeps = filter (importNotNative) startDeps
    nextLevelDeps = concat $ map (\libName -> internalDeps Map.! libName) notNativeStartDeps
    uniqueDeps = List.nub $ startDeps ++ nextLevelDeps
  in 
    if (length uniqueDeps == length startDeps)
    then uniqueDeps
    else traverseDeps uniqueDeps
    

stdLibDeps :: [String] -> Either String [Name]
stdLibDeps modules = do
    topLevelDeps <- (List.nub . concat) `fmap` mapM moduleStdlibDeps modules
    let topWithDefaults = List.nub (topLevelDeps ++ defaultImports)
    --Get dependencies of dependencies
    return $ traverseDeps topLevelDeps

nativesForSources :: [String] -> Either String (Map.Map Name String)
nativesForSources modules = do
    libDeps <- stdLibDeps modules
    let nativeNames = List.filter (not . importNotNative ) libDeps
    return $ Map.filterWithKey (\d _ -> List.elem d nativeNames) nativeDict