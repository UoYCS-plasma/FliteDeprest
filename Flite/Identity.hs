module Flite.Identity where

import Control.Applicative (Applicative(..))

newtype Identity a = I { runIdentity :: a }

instance Functor Identity where
  fmap f (I a) = I $ f a

instance Applicative Identity where
  pure a = I a

  (I f) <*> (I a) = I $ f a

instance Monad Identity where
  return a = I a
  I a >>= f = f a
