module Language.PP where

import Control.Monad
import Control.Monad.Free
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
mu B = 3

go :: MonadRandom m => Double -> Y -> S -> PP m S
go theta y s = do
  s' <- liftF $ categorical [(theta, s), (1 - theta, switch s)]
  observe (py s' y)
  return s'

ys :: [Double]
ys = [3, 0, 3, 0]

model :: MonadRandom m => Double -> PP m [S]
model theta = sequence . scanl (>>=) (pure A) $ map (go theta) ys

-- Util to get result
vote :: Ord a => [[a]] -> [a]
vote xxs =  map (snd . f) (transpose xxs)
  where
    f = maximum . map (\g -> (length g, head g))
      . groupBy (==) . sort

-- Run bootstrap with 100 particles over model
bsmain =
  eval (bootstrap 100 (model 0.5)) >>= print . vote . map snd . snd

