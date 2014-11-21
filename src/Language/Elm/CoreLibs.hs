 {-# LANGUAGE TemplateHaskell #-}
 module Language.Elm.CoreLibs where
   
import Data.FileEmbed
import Language.Elm.BuildUtil

import Elm.Compiler
import Elm.Compiler.Module

import qualified Data.Map as Map
import Control.Monad

import Data.ByteString.Char8 (unpack)

import Data.Maybe (fromJust)

header :: String
header =
   "var Elm = Elm || { Native: {} };\n"
  
runtime = (fromJust $ nameFromString "Runtime", unpack $(embedFile  "core/src/Native/Runtime.js") )

nativeDict = Map.fromList nativePairs
  where 
    nativePairs = map (\(n,t) -> (fromJust $ nameFromString n, unpack t)) nativeTextPairs
    nativeTextPairs = [
        ("Native.Array", $(embedFile  "core/src/Native/Array.js"))
        , ("Native.Basics", $(embedFile  "core/src/Native/Basics.js"))
        , ("Native.Bitwise", $(embedFile  "core/src/Native/Bitwise.js"))
        , ("Native.Char", $(embedFile  "core/src/Native/Char.js"))
        , ("Native.Color", $(embedFile  "core/src/Native/Color.js"))
        , ("Native.Date", $(embedFile  "core/src/Native/Date.js"))
        , ("Native.Debug", $(embedFile  "core/src/Native/Debug.js"))
        , ("Native.Http", $(embedFile  "core/src/Native/Http.js"))
        , ("Native.Json", $(embedFile  "core/src/Native/Json.js"))
        , ("Native.Keyboard", $(embedFile  "core/src/Native/Keyboard.js"))
        , ("Native.List", $(embedFile  "core/src/Native/List.js"))
        , ("Native.Mouse", $(embedFile  "core/src/Native/Mouse.js"))
        , ("Native.Ports", $(embedFile  "core/src/Native/Ports.js"))
        , ("Native.Regex", $(embedFile  "core/src/Native/Regex.js"))
        , ("Native.Show", $(embedFile  "core/src/Native/Show.js"))
        , ("Native.Signal", $(embedFile  "core/src/Native/Signal.js"))
        , ("Native.String", $(embedFile  "core/src/Native/String.js"))
        , ("Native.Text", $(embedFile  "core/src/Native/Text.js"))
        , ("Native.Time", $(embedFile  "core/src/Native/Time.js"))
        , ("Native.Touch", $(embedFile  "core/src/Native/Touch.js"))
        , ("Native.Trampoline", $(embedFile  "core/src/Native/Trampoline.js"))
        , ("Native.Transform2D", $(embedFile  "core/src/Native/Transform2D.js"))
        , ("Native.Utils", $(embedFile  "core/src/Native/Utils.js"))
        , ("Native.WebSocket", $(embedFile  "core/src/Native/WebSocket.js"))
        , ("Native.Window", $(embedFile  "core/src/Native/Window.js"))
        
        , ("Native.Graphics.Collage", $(embedFile  "core/src/Native/Graphics/Collage.js"))
        , ("Native.Graphics.Element", $(embedFile  "core/src/Native/Graphics/Element.js"))
        , ("Native.Graphics.Input", $(embedFile  "core/src/Native/Graphics/Input.js"))


        ]

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
        (_, ifaces) <- compileAll "elm-lang" "core" Map.empty sources
        return ifaces
     --jsSources = 
     
     