{-# LANGUAGE TupleSections #-}
module Language.PP.Examples.StickyState where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict hiding (get)
import Control.Monad.Trans.State.Strict
import Control.Monad.Free

import Data.List

import Language.PP.Types
import Language.PP.Dist
import Language.PP.Eval
import Language.PP.Infer
import Language.PP.Infer.Bootstrap (bootstrap)

import Data.Random (MonadRandom, logPdf)
import Data.Random.Distribution.Normal

import System.Random

import Data.List

-- TODO: description

-- | A binary hidden state
data S = A | B deriving(Eq, Read, Show, Ord)

-- | The state outputs a gaussian distributed 'Double'
type Y = Double

-- | Given a state, return the other state.
switch :: S -> S
switch A = B
switch B = A

-- | Probability of an observation given we're in some state 'S'
-- that is: P( y | s )
py :: S -> Y -> Double
py s y = logPdf (Normal (mu s) 1) y

-- | The mean for a given state
mu :: S -> Double
mu A = 0
mu B = 5

-- | One "step" of the model. This takes a previous state and outputs a current state and an observation
go :: MonadRandom m => Double -> (S -> PP m Y) -> S -> PP m (Y, S)
go theta obs s = do
  s' <- liftF $ categorical [(theta, s), (1 - theta, switch s)]
  y  <- obs s'
  return (y, s')

-- | Chain together a list of stages in the model
chain' :: MonadRandom m => [S -> PP m (Y, S)] -> S -> PP m [(Y, S)]
chain' []     s = return []
chain' (f:fs) s = do
  (y, s') <- f s
  rest    <- chain fs s'
  return  $! (y, s) : rest

flipTuple (x, y) = (y, x)

chain fs s = evalStateT (chain2 $ map StateT fs) s

chain2 :: Monad m => [StateT s m a] -> StateT s m [(a, s)]
chain2 = sequence . map viewState
  where
    toStates :: Monad m => [s -> m (a, s)] -> [StateT s m a]
    toStates = map StateT

    -- view states as well as outputs
    viewState :: Monad m => StateT s m a -> StateT s m (a, s)
    viewState m = do
      s <- get -- view state prior to StateT running
      (, s) <$!> m

-- | Put all the steps of the model together.
-- Generates either N observations (Left n),
-- or evaluates probability with given observations (Right ys)
--
-- NOTE: always starts in state 'A'
model :: MonadRandom m => Either Int [Y] -> Double -> PP m [(Y, S)]
model f theta = chain (either (generate theta) (observed theta) f) A

-- | Actually generate observations
generate :: MonadRandom m => Double -> Int -> [S -> PP m (Y, S)]
generate theta n = replicate n (f theta)
  where f theta = go theta (\s -> liftF $ gaussian (mu s) 1)

-- | Use pre-observed data as the observations
observed :: MonadRandom m => Double -> [Double] -> [S -> PP m (Y, S)]
observed theta ys = map (go theta . obs) ys
  where obs y s = observe (py s y) >> return y

-- | P(theta)
prior :: MonadRandom m => PP m Double
prior = liftF $ uniform 0 1

{-f = runIdentity . flip runStateT (mkStdGen 0)-}
f = flip evalState (mkStdGen 0)

-- Generate data to learn from
generateData theta numObs = do
  (ys', ss) <- fmap (unzip . snd) $ eval (model (Left numObs) theta)
  {-let (ys', ss) = (unzip . snd . f) $ eval (model (Left numObs) theta)-}
  let ys        = drop 1 ys'
  print ss
  print $ map round ys'
  putStrLn $ replicate 20 '-'
  return (ss, ys)

main = fullMain -- testBootstrap

testBootstrap = do
  let theta  = 0.75
      numObs = 300
  (ss, ys) <- generateData theta numObs
  (_, r) <- eval . bootstrap 300 $ model (Right ys) theta
  {-print (map snd r)-}
  putStrLn "done"

fullMain = do
  -- generate observations with some made-up theta
  let theta  = 0.75
      numObs = 40
  (ss, ys) <- generateData theta numObs

  -- Use PMMH to estimate the joint distribution of (theta, [S])
  {-let (_, rs) = f $ eval (pmmh 2000 20 (prior, model (Right ys)))-}
  (_, rs) <- eval (pmmh 2000 20 (prior, model (Right ys)))
  print theta
  print (estimateTheta . take 500 $ rs)
  -- note: this is a really naive way to estimate [S] but it works :-)
  print $ let (_, _, _, ss) = head rs in map fst ss

-- | Take the mean of MCMC samples
estimateTheta :: [(a, Double, b, c)] -> Double
estimateTheta xs =
  let ts = map f xs in sum ts / fromIntegral (length ts)
  where f (_, x, _, _) = x
