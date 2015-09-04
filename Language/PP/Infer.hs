{-# LANGUAGE TupleSections #-}
module Language.PP.Infer where

import Control.Monad
import Control.Monad.Free

import Language.PP.Types
import Language.PP.Dist (categorical)
import Language.PP.Eval (eval)

import Data.Random

-- | Step an interpreter once, weighting by its probability
step :: Monad m => PP m a -> m (Double, PP m a)
step (Free (P m)) = m
step (Pure x)     = return (0, return x)
-- Final step repeats with probability 1 forever (termination state)

-- | Resample a set of interpreters by simulating a step
-- and picking the highest weights
resample :: MonadRandom m => [PP m a] -> P m (PP m a)
resample xs = P $ mapM (fmap unlog . step) xs >>= runP . categorical
  where unlog (p, x) = (exp p, x) -- categorical needs non-log weights

terminated :: PP m a -> Bool
terminated (Free x) = False
terminated (Pure x) = True

-- Yeah, yeah. Only call this once you've checked. etc. etc.
getPure :: PP m a -> a
getPure (Pure x) = x
getPure (Free x) = error "don't call getPure on Free!"

bootstrap :: MonadRandom m => Int -> PP m a -> PP m [(Probability, a)]
bootstrap n prog = loopstrap n (replicate n prog)

-- | distribution over weighted particles
loopstrap :: MonadRandom m => Int -> [PP m a] -> PP m [(Probability, a)]
loopstrap n ps
  | all terminated ps = f ps
  | otherwise = replicateM n (liftF $ resample ps) >>= loopstrap n

f :: MonadRandom m => [PP m a] -> PP m [(Probability, a)]
f = g . sequence . map eval

g :: MonadRandom m => m [(Probability, a)] -> PP m [(Probability, a)]
g = liftF . P . fmap (0, )

-- A distribution over final results
{-valueBootstrap :: MonadRandom m => Int -> PP m a -> PP m [a]-}
{-valueBootstrap n prog = valueLoopstrap n (replicate n prog)-}

{--- | Run all interpreters and resample the "fittest" ones-}
{---   until they're all finished-}
{-valueLoopstrap :: MonadRandom m => Int -> [PP m a] -> PP m [a]-}
{-valueLoopstrap n ps-}
  {-| all terminated ps = return $ map getPure ps-}
  {-| otherwise = replicateM n (liftF $ resample ps) >>= valueLoopstrap n-}

