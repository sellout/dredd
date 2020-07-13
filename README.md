# Dredd

"I _am_ the law!" ⸻[Judge Dredd](https://en.wikipedia.org/wiki/Judge_Dredd)

This is a GHC plugin that, during compilation, automatically builds a
[hedgehog-classes](https://github.com/hedgehogqa/haskell-hedgehog-classes) test
suite for the type class instances defined in each module.

## Usage

You need to add this plugin to any library you want to test, then you have to
add a corresponding test suite. Here is a cabal example:

```
name:                  my-library

library
  exposed-modules:     My.Test.Mod
  ...
  ghc-options:         -fplugin Dredd

library my-library-testing -- where `Gen`s and `Laws` are defined
  exposed-modules:     My.Test.Mod.Gen,
                       My.Test.Mod.Laws
  ...

test-suite my-test
  build-depends:       base,
                       my-library,
					   my-library-testing,
                       hedgehog-classes
  default-language:    Haskell2010
  hs-source-dirs:      dredd
  main-is:             Main.hs                 -- you have to write this one
  other-modules:       Judge.Dredd.My.Test.Mod -- this module is autogenerated
  type:                exitcode-stdio-1.0
```

`Main.hs` should look like
```haskell
{-# options_ghc -F -pgmF lawmaster #-}
```

### NOTE

To test this package, it's necessary to do `cabal install && cabal test` because
the tests require `lawmaster` to be installed.

## Caveats

Currently, it expects things to be named in a particular way:

- `Gen` and `Laws` need to be defined in a separate library, so they don't get recursively processed, looking for more laws to check;
- `Gen`s need to be named `gen<Type>` and live in `<defining module>.Gen`, and must expect an `m a` for each type parameter;
- `Laws` need to be named `<Class>Laws` and live in `<defining module>.Laws`;
- the test suite needs to have `hs-source-dirs: dredd` (since that's where the generated modules will be put);
- the test suite needs to have `main-is: Main.hs` ("dredd/Main.hs" needs to contain the `options_ghc` pragma);

Here is a brief example:
```haskell
module MyModule where

instance MyTypeClass MyType
```
will generate a test module that looks like
```haskell
module Test.Dredd.MyModule (dreddLaws) where

import Hedgehog.Classes
import MyModule.Gen

dreddLaws :: [(String, [Laws])]
dreddLaws =
  [ ("MyType", [myTypeClassLaws genMyType]),
    ...
  ]
```
and the generated driver looks something like
```haskell
module Main (main) where

import Data.Functor (void)
import Hedgehog.Classes (lawsCheckmMany)
import qualified Judge.Dredd.MyModule.Gen

main :: IO ()
main = void . lawsCheckMany $ mconcat [Judge.Dredd.MyModule.Gen.dreddLaws, ...]
```

## ToDo

- omit tests that aren't satisfied (e.g., most `Laws` require `Eq` and `Show` instances, and we shouldn't generate tests for types that are missing those instances (or maybe create orphan standalone deriving instances in the generated test suite)
- support parameterized instances (e.g., `Monoid a => Monoid (Maybe a)`), perhaps using [Exemplar](https://github.com/matt-noonan/exemplar)
- allow configuration of names and locations for `Laws` and `Gen`s (similarly, support other test frameworks).
