 {-# LANGUAGE TemplateHaskell #-}
module Language.Elm.CoreLibs where

import Data.FileEmbed

arraySrc =  $(embedFile  "core/src/Array.elm")