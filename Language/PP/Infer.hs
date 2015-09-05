{-# LANGUAGE TupleSections #-}
module Language.PP.Infer where

import Control.Monad
import Control.Monad.Free

import Language.PP.Types
import Language.PP.Dist (categorical)
import Language.PP.Eval (eval)

import Data.Random

-- | True if a program is terminated (i.e. is 'Pure')
terminated :: PP m a -> Bool
terminated (Free x) = False
terminated (Pure x) = True

-- | Unsafely extract a 'Pure' value
getPure :: PP m a -> a
getPure (Pure x) = x
getPure (Free x) = error "don't call getPure on Free!"

-- | Step a program once, returning a log-weight
step :: Monad m => Double -> PP m a -> m (Double, PP m a)
step p (Free (P m)) = m
step p (Pure x)     = return (p, return x)

-- | Step a list of weighted programs, replacing their weights
-- if they are not terminated.
stepAll
  :: MonadRandom m
  => [(Double, PP m a)]
  -> PP m [(Double, PP m a)]
stepAll ps = liftF . P $ f <$> mapM (uncurry step) ps
        -- Double of bootstrap filter itself is sum of child
        -- probabilities
  where f ps = (sum (map fst ps), ps)

-- | Resample a list of weighted programs (By their log weights).
resample
  :: MonadRandom m
  => Int
  -> [(Double, PP m a)]
  -> PP m [(Double, PP m a)]
resample n = replicateM n . liftF . categorical . fmap f
  where f x = (exp (fst x), x)

bootstrap :: MonadRandom m => Int -> PP m a -> PP m [(Double, a)]
bootstrap n prog = loop (replicate n (0.0, prog))
  where loop ps
          | all (terminated . snd) ps = return . map (\(p, x) -> (p, getPure x)) $ ps
          | otherwise = stepAll ps >>= resample n >>= loop
        f (p, x) = (p, getPure x)
