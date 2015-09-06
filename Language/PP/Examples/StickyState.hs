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

-- | Put all the steps of the model together.
-- Generates either N observations (Left n), or fixes observations
-- to be "ys".
--
-- NOTE: always starts in state 'A'
model :: MonadRandom m => Either Int [Y] -> Double -> PP m [(S, Y)]
model f theta = sequence . scanl (>>=) (pure (A, 0)) . map (.fst)
              $ either (generate theta) (observed theta) f

generate :: MonadRandom m => Double -> Int -> [S -> PP m (S, Y)]
generate theta n = replicate n (f theta)
  where f theta = go theta (\s -> liftF $ gaussian (mu s) 1)

observed :: MonadRandom m => Double -> [Double] -> [S -> PP m (S, Y)]
observed theta ys = map (go theta . obs) ys

obs y s = observe (py s y) >> return y

prior :: MonadRandom m => PP m Double
prior = liftF $ uniform 0 1

main = do
  -- | generate observations with some made-up theta (0.8)
  let theta  = 0.99
      numObs = 10
  (ss, ys') <- (unzip . snd) <$> eval (model (Left numObs) theta)
  let ys = drop 1 ys'
  mapM_ print (zip ss ys')
  putStrLn $ replicate 20 '-'

  rs <- eval (pmmh 1000 10 (prior, model (Right ys)))
  mapM_ print . take 500 . snd $ rs
  

{--- Util to get result-}
{-vote :: Ord a => [[a]] -> [a]-}
{-vote xxs =  map (snd . f) (transpose xxs)-}
  {-where-}
    {-f = maximum . map (\g -> (length g, head g))-}
      {-. groupBy (==) . sort-}

{--- Asumming we know theta, run bootstrap with 100 particles over model-}
{-bsmain = eval (bootstrap 100 (model 0.7)) >>= print . vote . map snd . snd-}

{-modelPmmh :: MonadRandom m => PMMH m Double [S]-}
{-modelPmmh = (prior, model)-}

{-est :: [(a, Double, b, c)] -> Double-}
{-est xs = let ts = map f xs in sum ts / 500.0-}
  {-where f (_, x, _, _) = x-}

{--- | Run a 1000-iteration PMMH sampler with 500 sample burn-in-}
{--- using 10 particles, and print the trace.-}
{-main = eval (pmmh 1000 10 modelPmmh) >>= mapM_ print . drop 500 . snd-}
