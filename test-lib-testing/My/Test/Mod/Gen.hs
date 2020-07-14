module My.Test.Mod.Gen where

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

import My.Test.Mod

genSimple :: Hedgehog.MonadGen m => m Simple
genSimple = Gen.element [A, B]

genMeh :: Hedgehog.MonadGen m => m Meh
genMeh = Meh <$> Gen.maybe genSimple

genMep :: Hedgehog.MonadGen m => m a -> m (Mep a)
genMep a = Mep <$> Gen.maybe a
