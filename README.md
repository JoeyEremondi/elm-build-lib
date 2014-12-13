elm-build-lib
=============

A library for compiling Elm to JavaScript from within Haskell

This library provides both runtime and Template Haskell functions which let you take multi-module Elm Programs
and compile them to JavaScript. 
The main goal of this is to allow Elm to be used as a frontend for Haskell servers or Web apps. 
The library is independent of any specific framework, so it should work with Yesod, Snap, Happstack, Scotty, etc. 

You can find this package on [Hackage](http://hackage.haskell.org/package/elm-build-lib).

##Examples

**Compiling to a single JS file**

```haskell
import Language.Elm.Build

mainSource = "module Main where\nx=3"
fooSource = "module Foo where\nx=3"

myJS :: String
myJS = compileAndLinkAll [mainSource, fooSource]
```

Of course, you can load the elm source from a file, or use a nicer multi-line
string library such as QuasiText.

The order you give the modules in does not matter.

**Compiling to a single JS file, during Haskell compilation**

```haskell
import Language.Elm.Build

mainSource = "module Main where\nx=3"
fooSource = "module Foo where\nx=3"

myJS :: String
myJS = $(deriveElmJS [mainSource, fooSource])
```

This uses TemplateHaskell to do the Elm->JS conversion when Haskell compiles, so
that you don't recompile your code every time you serve its JavaScript.
