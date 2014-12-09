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

stdLibDeps :: [String] -> Either String [Name]
stdLibDeps modules = do
    topLevelDeps <- (List.nub . concat) `fmap` mapM moduleStdlibDeps modules
    --Get dependencies of dependencies
    let allDeps =  concat $ map (\libName -> internalDeps Map.! libName) topLevelDeps
    return $ List.nub $ topLevelDeps ++ allDeps

nativesForSources :: [String] -> Either String (Map.Map Name String)
nativesForSources modules = do
    libDeps <- stdLibDeps modules
    let nativeNames = List.filter (not . importNotNative ) libDeps
    return $ Map.filterWithKey (\d _ -> List.elem d nativeNames) nativeDict