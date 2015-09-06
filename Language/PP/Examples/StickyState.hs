module Language.PP.Examples.StickyState where
import Control.Monad.Free
import Language.PP.Types
import Language.PP.Dist
import Language.PP.Eval
import Language.PP.Infer

import Data.Random (MonadRandom, logPdf)
import Data.Random.Distribution.Normal

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
go :: MonadRandom m => Double -> (S -> PP m Y) -> S -> PP m (S, Y)
go theta obs s = do
  s' <- liftF $ categorical [(theta, s), (1 - theta, switch s)]
  y  <- obs s'
  return (s', y)

-- | Chain together a list of stages in the model
chain :: MonadRandom m => [S -> PP m (S, Y)] -> S -> PP m [(S, Y)]
chain []     s = return []
chain (f:fs) s = do
  (s', y) <- f s
  rest    <- chain fs s'
  return  $ (s, y) : rest

-- | Put all the steps of the model together.
-- Generates either N observations (Left n),
-- or evaluates probability with given observations (Right ys)
--
-- NOTE: always starts in state 'A'
model :: MonadRandom m => Either Int [Y] -> Double -> PP m [(S, Y)]
model f theta = chain (either (generate theta) (observed theta) f) A

-- | Actually generate observations
generate :: MonadRandom m => Double -> Int -> [S -> PP m (S, Y)]
generate theta n = replicate n (f theta)
  where f theta = go theta (\s -> liftF $ gaussian (mu s) 1)

-- | Use pre-observed data as the observations
observed :: MonadRandom m => Double -> [Double] -> [S -> PP m (S, Y)]
observed theta ys = map (go theta . obs) ys
  where obs y s = observe (py s y) >> return y

-- | P(theta)
prior :: MonadRandom m => PP m Double
prior = liftF $ uniform 0 1

main = do
  -- generate observations with some made-up theta
  let theta  = 0.5
      numObs = 30
  (ss, ys') <- (unzip . snd) <$> eval (model (Left numObs) theta)
  let ys = drop 1 ys'
  print ss
  print $ map round ys'
  putStrLn $ replicate 20 '-'

  -- Use PMMH to estimate the joint distribution of (theta, [S])
  (_, rs) <- eval (pmmh 1500 20 (prior, model (Right ys)))
  print theta
  print (estimateTheta $ take 300 rs)
  -- note: this is a really naive way to estimate [S] but it works :-)
  print $ let (_, _, _, ss) = head rs in map fst ss

-- | Take the mean of MCMC samples
estimateTheta :: [(a, Double, b, c)] -> Double
estimateTheta xs =
  let ts = map f xs in sum ts / fromIntegral (length ts)
  where f (_, x, _, _) = x
