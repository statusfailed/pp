{-# LANGUAGE BangPatterns #-}
module Language.PP.Infer.Bootstrap where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Language.PP.Types
import Language.PP.Dist (categorical)
import Language.PP.Eval (eval, withP)

import Data.Random (MonadRandom)

-- | True if a program is terminated (i.e. is 'Pure')
terminated :: PP m a -> Bool
terminated (Free x) = False
terminated (Pure x) = True

-- | Unsafely extract a 'Pure' value
getPure :: PP m a -> a
getPure (Pure !x) = x
getPure (Free !x) = error "don't call getPure on Free!"

-- | Step a program once, returning a log-weight
step :: Monad m => Double -> PP m a -> m (Double, PP m a)
step p (Free (P m)) = f <$!> m
  where f (p', x) = (p + p', x)
step p x@(Pure _)     = return (p, x)

-- | Step a list of weighted programs, replacing their weights
-- if they are not terminated.
stepAll
  :: MonadRandom m
  => [(Double, PP m a)]
  -> PP m [(Double, PP m a)]
stepAll !ps = liftF . P $ f <$!> mapM (uncurry step) ps
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

checkDone :: Monad m => StateT [(Double, PP t a)] m Bool
checkDone = do
  particles <- get
  return $ all (terminated . snd) particles

advance :: MonadRandom m => Int -> StateT [(Double, PP m a)] (PP m) ()
advance n = do
  ps <- get
  r  <- lift (stepAll ps >>= resample n)
  put r

{-bootstrap' :: MonadRandom m => Int -> PP m a -> PP m [(Double, a)]-}
bootstrap' :: MonadRandom m => Int -> StateT [(Double, PP m a)] (PP m) ()
bootstrap' n = do
  isDone <- checkDone
  unless isDone $ do
    advance n
    bootstrap' n

bootstrap :: MonadRandom m => Int -> PP m a -> PP m [(Double, a)]
bootstrap n prog = f $ execStateT (bootstrap' n) (replicate n (0.0, prog))
  where f = fmap $ map (fmap getPure)

{-bloop :: MonadRandom m => Int -> StateT [Particle m a] (PP m-}
{-bloop :: MonadRandom m => Int -> [PP m (Double, a)] -> PP m [(Double, a)]-}

{-bootstrap :: MonadRandom m => Int -> PP m a -> PP m [(Double, a)]-}
{-bootstrap n prog = loop (replicate n (0.0, prog))-}
  {-where loop !ps-}
          {-| all (terminated . snd) ps = return $ f <$!> ps-}
          {-| otherwise = stepAll ps >>= resample n >>= loop-}
        {-f (p, x) = (p, getPure x)-}

--  loop ps = case terminated ps of
--    Just yes -> yes
--    Nothing  -> stepAll ps >>= resample n >>= loop
