module Flite.Fresh where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM, replicateM)

data Fresh a = Fresh { runFresh :: String -> Int -> (Int, a) }

data FreshW a b = FreshW { runFreshW :: String -> Int -> ([a], Int, b) }

instance Functor (FreshW a) where
  fmap = liftM

instance Applicative (FreshW a) where
  pure  = return
  (<*>) = ap

instance Monad (FreshW a) where
  return a = FreshW (\s i -> ([], i, a))
  m >>= f  = FreshW (\s i -> case runFreshW m s i of
                                (l, j, a) -> let (l', j', a') = runFreshW (f a) s j
                                             in (l ++ l', j', a'))

instance Functor Fresh where
  fmap = liftM

instance Applicative Fresh where
  pure  = return
  (<*>) = ap

instance Monad Fresh where
  return a = Fresh (\s i -> (i, a))
  m >>= f  = Fresh (\s i -> case runFresh m s i of
                              (j, a) -> runFresh (f a) s j)

fresh :: Fresh String
fresh = Fresh (\s i -> (i+1, s ++ show i))

askFresh :: Fresh String
askFresh = Fresh (\s i -> (i, s))

newScope :: String -> Fresh a -> Fresh a
newScope s m = Fresh (\_ i -> runFresh m s i)

freshList :: Int -> Fresh [String]
freshList n = replicateM n fresh

freshW :: FreshW a String
freshW = FreshW (\s i -> ([], i+1, s ++ show i))

askFreshW :: FreshW a String
askFreshW = FreshW (\s i -> ([], i, s))

newScopeW :: String -> FreshW w a -> FreshW w a
newScopeW s m = FreshW (\_ i -> runFreshW m s i)

freshListW :: Int -> FreshW w [String]
freshListW n = replicateM n freshW

writeF :: w -> FreshW w ()
writeF w = FreshW (\s i -> ([w], i, ()))

writeManyF :: [w] -> FreshW w ()
writeManyF ws = FreshW (\s i -> (ws, i, ()))
