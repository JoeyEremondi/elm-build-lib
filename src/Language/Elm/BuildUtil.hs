{-# LANGUAGE TemplateHaskell #-}
module Language.Elm.BuildUtil where

import qualified Data.Map as Map

import Elm.Compiler
import Elm.Compiler.Module
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad (filterM)
import Data.Maybe (fromJust)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote 

elmModuleName :: String -> Either String Elm.Compiler.Module.Name
elmModuleName modul = do
    (name, _) <- parseDependencies modul
    return name