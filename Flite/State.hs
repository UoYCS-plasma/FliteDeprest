module Flite.State where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM)

newtype State s a = S { runState :: s -> (s, a) }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = S (\s -> (s, a))
  m >>= f = S (\s -> case runState m s of
                       (s', a) -> runState (f a) s')
