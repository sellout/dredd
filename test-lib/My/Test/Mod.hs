{-# language FlexibleInstances #-}

module My.Test.Mod where

data Simple = A | B
  deriving (Eq, Show)

instance Semigroup Simple where
  A <> A = A
  _ <> _ = B

newtype Meh = Meh (Maybe Simple)
  deriving (Eq, Show)

instance Semigroup Meh where
  x <> _ = x

-- newtype Mep a = Mep (Maybe a)
-- --   deriving (Eq, Show)

-- instance Semigroup (Mep Simple) where
--   x <> _ = x
