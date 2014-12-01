module Language.Elm.Build (
  compileAll 
  , standalone
  , addHeader
  ,deriveElmJS
  , elmQuasi
  ) where

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

{-|
Given a list of strings containing the source code for elm modules,
return a dictionary mapping names to their compiled JavaScript source.
(This allows you to staticaly serve commonly used modules, such as the runtime).
The runtime is included in this dictionary, with the key `Runtime`.
Gives a string error in the event of failure.
-}
compileAll ::  [String] -> Either String (Map.Map Module.Name String)
compileAll modules = do
  (sources, _ifaces) <- Util.compileAll "" "" stdLib modules
  let sourcesWithNatives = Map.insert (fst runtime) (snd runtime) (Map.union sources nativeDict)
  return sourcesWithNatives

  
{-|
Bundle the result of compilation into a single, standalone, JavaScript source file,
including the Elm-runtime and the Elm header
|-}
standalone :: (Map.Map Module.Name String) -> String
standalone result = addHeader $ concatMap snd $ Map.toList result

{-|
Add the JavaScript header which initializes the Elm object
and allows sources to work together.
|-}
addHeader :: String -> String
addHeader  = (header ++)


elmQuasi :: QuasiQuoter
elmQuasi = QuasiQuoter { quoteExp = \s -> deriveElmJS [s],
                       quotePat = \_s -> error "Can't use Elm quasiQuoter in pattern position",
                       quoteType = \_s -> error "Can't use Elm quasiQuoter in type position",
                       quoteDec = \_s -> error "Can't use Elm quasiQuoter in dec position"
                       }  
  
deriveElmJS :: [String] -> TH.ExpQ
deriveElmJS modules = case (standalone `fmap` compileAll modules) of
    Right s -> TH.litE $ TH.stringL s
    Left err -> error $ "Error compiling Elm code:\n" ++ err




          
    
