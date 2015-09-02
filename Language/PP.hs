module Language.PP where

import Control.Monad
import Control.Monad.Free
import Language.PP.Types
import Language.PP.Dist
import Language.PP.Infer

import Data.Random
import Data.Random.Distribution.Normal

import Data.List

-- | Declare an observation with weight 'p'
observe :: Monad m => Double -> PP m ()
observe p = liftF . P . return $ (p, ())

prog :: PP IO Double
prog = do
  x <- liftF $ gaussian 0 1
  y <- liftF $ gaussian 0 1
  return $ x + y


-- | Generate data from the model
gen :: PP IO Double -> IO Double
gen (Pure x)     = return x
gen (Free (P m)) = m >>= (gen . snd)

-- | Generate data + probability from the model
eval :: Monad m => PP m a -> m (Probability, a)
eval prog = go 0 prog
  where
    go p (Free (P m)) = m >>= (\(p', x) -> go (p + p') x)
    go p (Pure x)     = return (p, x)

chain :: Monad m => [a -> m a] -> a -> m a
chain []     a = return a
chain (f:fs) a = f a >>= chain fs 

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
mu B = 100

go :: MonadRandom m => Double -> Y -> S -> PP m S
go theta y s = do
  s' <- liftF $ categorical [(theta, s), (1 - theta, switch s)]
  observe (py s' y)
  return s'

ys :: [Double]
ys = [0, 100, 0, 100, 0, 100, 0, 100]

model :: MonadRandom m => Double -> PP m [S]
model theta = sequence . scanl (>>=) (pure A) $ map (go theta) ys

-- Util to get result
vote :: Ord a => [[a]] -> [a]
vote xxs =  map (snd . f) (transpose xxs)
  where
    f = maximum . map (\g -> (length g, head g))
      . groupBy (==) . sort

-- Run bootstrap with 100 particles over model
main = eval (bootstrap 100 (model 0.5)) >>= print . vote . snd
