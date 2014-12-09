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
   

stdLib :: CompileResult
stdLib  = case eitherLib of
  Left s -> error $ "Failed building standard library: " ++ s
  Right dict -> dict -- Map.filterWithKey (\ d _ -> List.elem d deps) dict
  where 
      eitherLib = compileAll "elm-lang" "core" (Map.empty, Map.empty) sources
     --jsSources = 
     
     