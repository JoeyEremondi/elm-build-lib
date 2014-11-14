 {-# LANGUAGE TemplateHaskell #-}
 module Language.Elm.CoreLibs where
   
import Data.FileEmbed
import Language.Elm.BuildUtil

import Elm.Compiler
import Elm.Compiler.Module

import qualified Data.Map as Map
import Control.Monad

import Data.ByteString.Char8 (unpack)
  

sources = map unpack [$(embedFile  "core/src/Array.elm")
   ,$(embedFile  "core/src/Array.elm")
   ,$(embedFile  "core/src/Basics.elm")
   ,$(embedFile  "core/src/Bitwise.elm")
   ,$(embedFile  "core/src/Char.elm")
   ,$(embedFile  "core/src/Color.elm")
   ,$(embedFile  "core/src/Date.elm")
   ,$(embedFile  "core/src/Debug.elm")
   ,$(embedFile  "core/src/Dict.elm")
   ,$(embedFile  "core/src/Http.elm")
   ,$(embedFile  "core/src/Keyboard.elm")
   ,$(embedFile  "core/src/List.elm")
   ,$(embedFile  "core/src/Maybe.elm")
   ,$(embedFile  "core/src/Mouse.elm")
   ,$(embedFile  "core/src/Random.elm")
   ,$(embedFile  "core/src/Regex.elm")
   ,$(embedFile  "core/src/Result.elm")
   ,$(embedFile  "core/src/Set.elm")
   ,$(embedFile  "core/src/Signal.elm")
   ,$(embedFile  "core/src/String.elm")
   ,$(embedFile  "core/src/Text.elm")
   ,$(embedFile  "core/src/Time.elm")
   ,$(embedFile  "core/src/Touch.elm")
   ,$(embedFile  "core/src/Trampoline.elm")
   ,$(embedFile  "core/src/Transform2D.elm")
   ,$(embedFile  "core/src/WebSocket.elm")
   ,$(embedFile  "core/src/Window.elm")
   
   ,$(embedFile  "core/src/Json/Encode.elm")
   ,$(embedFile  "core/src/Json/Decode.elm")
   
   ,$(embedFile  "core/src/Graphics/Collage.elm")
   ,$(embedFile  "core/src/Graphics/Element.elm")
   ,$(embedFile  "core/src/Graphics/Input.elm")
   
   ,$(embedFile  "core/src/Graphics/Input/Field.elm")
   ]

   
   
stdLib :: Map.Map Name Interface
stdLib = case eitherLib of
  Left s -> error $ "Failed building standard library: " ++ s
  Right dict -> dict
  where 
      eitherLib = do
        names <- mapM elmModuleName sources
        compiledList <- mapM (\s ->  compile "elm-lang" "core" s Map.empty) sources
        let ifaces = map fst compiledList
        let pairs = zip names ifaces
        return $ Map.fromList pairs
     --jsSources = 