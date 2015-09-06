module Main where

import Control.Monad.Free
import Language.PP
import Language.PP.Types
import Language.PP.Dist
import Language.PP.Eval
import Language.PP.Infer

import Data.Random (MonadRandom, logPdf)
import Data.Random.Distribution.Normal

import Data.List

type Y = Double

data S = A | B deriving(Eq, Read, Show, Ord)

switch :: S -> S
switch A = B
switch B = A

ptrans :: MonadRandom m => Double -> S -> PP m S
ptrans theta s =
  liftF $ categorical [(theta, s), (1 - theta, switch s)]

py :: S -> Y -> Double
py s y = logPdf (Normal (mu s) 1) y

mu :: S -> Double
mu A = 0
mu B = 5

go :: MonadRandom m => Double -> (S -> PP m Y) -> S -> PP m S
go theta obs s = do
  s' <- liftF $ categorical [(theta, s), (1 - theta, switch s)]
  obs s'
  {-observe (py s' y)-}
  return s'

ys :: [Double]
ys = [0, 0, 0, 0, 0, 5, 5, 5, 5, 5]

model :: MonadRandom m => Double -> PP m [S]
model theta =
  sequence . scanl (>>=) (pure A) $ map (go theta . obs) ys
  where obs y s = observe (py s y) >> return y

-- Util to get result
vote :: Ord a => [[a]] -> [a]
vote xxs =  map (snd . f) (transpose xxs)
  where
    f = maximum . map (\g -> (length g, head g))
      . groupBy (==) . sort

-- Run bootstrap with 100 particles over model
bsmain =
  eval (bootstrap 100 (model 0.9)) >>= print . vote . map snd . snd

prior :: MonadRandom m => PP m Double
prior = liftF $ uniform 0 1

modelPmmh :: MonadRandom m => PMMH m Double [S]
modelPmmh = (prior, model)

est :: [(a, Double, b, c)] -> Double
est xs = let ts = map f xs in sum ts / 500.0
  where f (_, x, _, _) = x

main = eval (pmmh 500 10 modelPmmh) >>= mapM_ print . drop 0 . snd

{-main = eval (pmmh 1000 10 modelPmmh) >>= print . est . drop 500 . snd-}
