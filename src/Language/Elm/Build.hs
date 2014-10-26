module Language.Elm.Build where

import Elm.Compiler
import Elm.Compiler.Module

x=3

elmModuleName :: String -> Either String Elm.Compiler.Module.Name
elmModuleName modul = do
    (name, _) <- parseDependencies modul
    return name