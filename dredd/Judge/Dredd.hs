module Main (main) where

import Data.Functor (void)
import Hedgehog.Classes
import qualified Judge.Dredd.My.Test.Mod

main :: IO ()
main = void . lawsCheckMany $ mconcat [Judge.Dredd.My.Test.Mod.dreddLaws]
