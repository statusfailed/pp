module Language.PP.Types
  ( Probability
  , P(..)
  , PP
  ) where

import Control.Monad.Free

type Probability = Double
type LogP        = Double

data P m a = P { runP :: m (Probability, a) }

instance Monad m => Functor (P m) where
  fmap f (P m) = P $ fmap (fmap f) m -- 2nd fmap is tuples

type PP m = Free (P m)
