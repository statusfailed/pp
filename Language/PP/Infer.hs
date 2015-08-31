module Language.PP.Infer where

import Control.Monad
import Control.Monad.Free
import Language.PP.Types

-- | Step an interpreter once, weighting by its probability
step :: Monad m => PP m a -> m (Double, PP m a)
step (Free (P m)) = m
step (Pure x)     = return (0, return x) -- TODO: is this right?

-- | Resample a set of interpreters by simulating a step
-- and picking the highest weights
resample :: [PP m a] -> m [PP m a]
resample xs = undefined
