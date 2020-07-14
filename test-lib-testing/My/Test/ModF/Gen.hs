module My.Test.ModF.Gen where

import qualified Hedgehog

import My.Test.ModF

genMyIdent :: Hedgehog.MonadGen m => m a -> m (MyIdent a)
genMyIdent = fmap MyIdent
