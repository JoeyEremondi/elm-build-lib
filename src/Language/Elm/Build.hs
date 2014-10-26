module Language.Elm.Build where

import qualified Data.Map as Map

import Elm.Compiler
import Elm.Compiler.Module

x=3

elmModuleName :: String -> Either String Elm.Compiler.Module.Name
elmModuleName modul = do
    (name, _) <- parseDependencies modul
    return name

compileInOrder :: [String] -> Either String String
compileInOrder modules = helper modules Map.empty ""
    where
      helper [] _ src = return src
      helper (modul:otherModules) compiledModules _ = do
          name <- elmModuleName modul
          (newModule, src) <- compile modul compiledModules
          let newCompiled = Map.insert name newModule compiledModules
          helper otherModules newCompiled src   
          
    