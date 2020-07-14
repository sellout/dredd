{-# language FlexibleInstances #-}

module My.Test.Mod where

data Simple = A | B
  deriving (Eq, Show)

instance Semigroup Simple where
  A <> A = A
  _ <> _ = B

instance Monoid Simple where
  mempty = A

newtype Meh = Meh (Maybe Simple)
  deriving (Eq, Show)

instance Semigroup Meh where
  x <> _ = x

newtype Mep a = Mep (Maybe a)
--   NB: These would have type vars and constraints, so we can't use them yet.
--   deriving (Eq, Show)

instance Eq (Mep Simple) where
  Mep x == Mep y = x == y

instance Show (Mep Simple) where
  show (Mep x) = show x

instance Semigroup (Mep Simple) where
  x <> _ = x
