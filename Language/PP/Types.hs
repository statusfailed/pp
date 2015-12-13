module Language.PP.Types
  ( Probability
  , P(..)
  , PP
  ) where

import Control.Monad.Free

type Probability = Double
type LogP        = Double

-- | A 'P m a' is a probabilistic value a, which uses a monad m as a source of
-- randomness.  it can be run (with 'runP' to produce a value and a
-- "Probability" of that value. 
-- NOTE: the "Probability" is not truly a probability, so won't sum to 1.
data P m a = P { runP :: m (Probability, a) }

instance Monad m => Functor (P m) where
  fmap f (P m) = P $ fmap (fmap f) m -- 2nd fmap is tuples

-- | A Probabilistic Program is just the combination of many probabilistic
-- values using Free.
type PP m = Free (P m)
