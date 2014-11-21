module Language.Elm.Build (compileAll, deriveElmJS) where

import qualified Data.Map as Map

import Elm.Compiler
import Elm.Compiler.Module
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad (filterM)
import Data.Maybe (fromJust)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote 

import qualified  Language.Elm.BuildUtil as Util
import Language.Elm.CoreLibs
import qualified Elm.Compiler.Module as Module

{-#
Given a list of strings containing the source code for elm modules,
return a string containing the compiled JavaScript source,
and a list of name-javascript pairs of the runtime/native modules 
the main source depends on.
Gives a string error in the event of failure.
-}
compileAll ::  [String] -> Either String (String, [(Module.Name, String)])
compileAll modules = do
  (src, _ifaces) <- Util.compileAll "" "" stdLib modules
  return (header ++ src, [runtime] ++ Map.toList nativeDict)
  
elmQuasi :: QuasiQuoter
elmQuasi = QuasiQuoter { quoteExp = \s -> deriveElmJS [s],
                       quotePat = \s -> TH.litP $ TH.stringL s,
                       quoteType = \s -> error "Can't use Elm quasiQuoter in type position",
                       quoteDec = \s -> error "Can't use Elm quasiQuoter in dec position"
                       }  
  
deriveElmJS :: [String] -> TH.ExpQ
deriveElmJS modules = case compileAll modules of
    Right (s, _) -> TH.litE $ TH.stringL s
    Left err -> error $ "Error compiling Elm code:\n" ++ err




          
    
