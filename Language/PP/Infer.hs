{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.PP.Infer where

import Control.Monad
import Control.Monad.Free

import Language.PP.Types
import Language.PP.Dist (categorical, uniform)
import Language.PP.Eval (eval, withP)

import Data.Random (MonadRandom)

import Language.PP.Infer.Bootstrap (bootstrap)

------------ PMMH ------------

type PMMH m theta a =
  ( PP m theta        -- prior
  , (theta -> PP m a) -- probabilistic program
  )

type Candidate theta a =
  ( Double  -- p(theta)
  , theta   -- theta
  , Double  -- p(a | theta)
  , a       -- a
  )

-- | Accept a new state with some probability (1 if p > 1)
accept :: MonadRandom m => Double -> a -> a -> PP m a
accept a x y = do
  p <- liftF (uniform 0 1)
  return $ if (a >= 1 || p <= a) then x else y

acceptCandidate :: MonadRandom m
  => Candidate theta a -- ^ proposed candidate
  -> Candidate theta a -- ^ previous candidate
  -> PP m (Candidate theta a) -- ^ resulting choice
acceptCandidate new@(pt, t, py, y) old@(pt', t', py', y')
  = accept a new old
  where num = py  + pt
        den = py' + pt'
        a = exp (num - den) -- MH accept ratio

propose
  :: MonadRandom m
  => Int
  -> PMMH m theta a
  -> PP m (Candidate theta a)
propose n (prior, prog) = do
  (pt, t) <- withP . P . eval $ prior
  (py, y) <- bootstrap n (prog t) >>= liftF . categorical . expWeights
  return (pt, t, py, y)
  where expWeights = map (\x@(w, _) -> (exp w, x))

pmmhStep :: MonadRandom m
         => Int
         -> PMMH m theta a
         -> Candidate theta a
         -> PP m (Candidate theta a)
pmmhStep n p@(prior, prog) c'@(_, t', _, _) =
  propose n p >>= (\c -> acceptCandidate c c')

-- Explicit looping here is a bit naff :<
pmmhLoop 0 _ _ _ = return []
pmmhLoop i n p c = do
  proposed <- propose n p
  next     <- acceptCandidate proposed c
  {-next <- pmmhStep n p c-}
  (next:) <$> pmmhLoop (i - 1) n p next

pmmh :: MonadRandom m
     => Int
     -> Int
     -> PMMH m theta a
     -> PP m ([Candidate theta a])
pmmh m n p = propose n p >>= pmmhLoop m n p
