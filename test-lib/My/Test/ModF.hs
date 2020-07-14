{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}

module My.Test.ModF where

newtype MyIdent a = MyIdent a
  deriving (Eq, Show, Foldable, Functor, Traversable)
