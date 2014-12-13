{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module      : Language.Elm.Build
Description : Compile Elm code to JS within Haskell
Copyright   : (c) Joey Eremondi 2014
License     : BSD3
Maintainer  : joey@eremondi.com
Stability   : experimental

This library provides both runtime and Template Haskell functions which let you take multi-module
Elm Programs and compile them to JavaScript.
The main goal of this is to allow Elm to be used as a frontend for Haskell servers or Web apps.
The library is independent of any specific framework, so it should work with Yesod, Snap, Happstack, Scotty, etc.
-}

module Language.Elm.Build (
  compileAll 
  , standalone
  , addHeader
  ,deriveElmJS
  , elmQuasi
  , compileAndLinkAll
  ) where

import qualified Data.Map as Map

import Elm.Compiler
import Elm.Compiler.Module
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad (filterM)
import Data.Maybe (fromJust)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote 

import qualified  Language.Elm.BuildUtil as Util
import Language.Elm.CoreLibs
import qualified Elm.Compiler.Module as Module

import Data.List (intercalate)


{-|
Given a list of strings containing the source code for Elm modules,
return a dictionary mapping names to their compiled JavaScript source.
(This allows you to statically serve commonly used modules, such as the runtime).
The runtime is included in this dictionary, with the key \"Elm.Native.Runtime\".
Gives a string error in the event of failure.
-}
compileAll ::  [String] -> Either String (Map.Map Module.Name String)
compileAll modules = do
  depPairs <- mapM Util.uniqueDeps modules
  let ourDeps  = Map.fromList depPairs
  let moduleDict = Map.fromList $ zip (map fst depPairs) modules
  ourStdlib <- stdLibForSources ourDeps modules
  ourNatives <- nativesForSources ourDeps
  (sources, _ifaces) <- Util.compileAll ourDeps "" "" ourStdlib moduleDict
  let sourcesWithNatives = Map.insert (fst runtime) (snd runtime) (Map.union sources ourNatives)
  return sourcesWithNatives

{-|
Given a list of strings containing the source code for Elm modules,
compile the modules and
bundle the result of compilation into a single, standalone, JavaScript source file,
including the Elm-runtime and the Elm header.
Gives a string error in the event of failure.
-}
compileAndLinkAll :: [String] -> Either String String
compileAndLinkAll modules = standalone `fmap` (compileAll modules)
  
{-|
Bundle the result of compilation into a single, standalone, JavaScript source file,
including the Elm-runtime and the Elm header.
-}
standalone :: (Map.Map Module.Name String) -> String
standalone result = 
    let
        sortedNames = List.sort $ map fst $ Map.toList result
        sortedSources = map (\n -> result Map.! n) sortedNames
    in addHeader $ intercalate "\n" sortedSources

{-|
Add the JavaScript header which initializes the Elm object
and allows sources to work together.
-}
addHeader :: String -> String
addHeader  = (header ++)


-- |
-- Derives the Template Haskell String literal corresponding to the compiled and linked
-- JavaScript for a given module.
-- For example:
-- 
-- > {-#Language QuasiQuotes#-}
-- > myElmJS :: String
-- > myElmJS = [elmQuasi|
--  >     module Main where
--  >    x = 3
-- > |]
-- 
-- 
-- This function is useful for small code snippets, but when dealing with
-- multi-module Elm projects, 'deriveElmJS' should be used.
-- Note that the Elm code is compiled to JS at when your Haskell code is compiled. 
elmQuasi :: QuasiQuoter
elmQuasi = QuasiQuoter { quoteExp = \s -> deriveElmJS [s],
                       quotePat = \_s -> error "Can't use Elm quasiQuoter in pattern position",
                       quoteType = \_s -> error "Can't use Elm quasiQuoter in type position",
                       quoteDec = \_s -> error "Can't use Elm quasiQuoter in dec position"
                       }  
                     
                       
-- |
-- Derives the Template Haskell String literal corresponding to the compiled and linked
-- JavaScript from compiling the given modules.
-- For example:
-- 
-- > {-# LANGUAGE TemplateHaskell #-}
-- > myElmJS :: String
-- > myElmJS = $(deriveElmJS [myElmMain, someElmHelperLib])
-- 
-- The main use of this is to ensure that your Elm code is compiled into JavaScript 
-- at the same time your Haskell code is compiled.
 
deriveElmJS :: [String] -> TH.ExpQ
deriveElmJS modules = case (compileAndLinkAll modules) of
    Right s -> TH.litE $ TH.stringL s
    Left err -> error $ "Error compiling Elm code:\n" ++ err




          
    
