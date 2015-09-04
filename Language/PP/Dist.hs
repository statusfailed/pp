module Language.PP.Dist
  ( gaussian
  , categorical
  ) where

import Language.PP.Types

import Data.Random
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Categorical hiding (categorical)
import Data.Random.Distribution.Uniform

import Data.Map as M (fromList, (!))

sampleP :: MonadRandom m => P (RVarT m) a -> P m a
sampleP = P . sample . runP

fromPDF :: (MonadRandom m, PDF d a) => d a -> P (RVarT m) a
fromPDF d = P . fmap (\x -> (logPdf d x, x)) . rvarT $ d

gaussian :: MonadRandom m => Double -> Double -> P m Double
gaussian mu sigma = sampleP $ fromPDF (Normal mu sigma)

-- 
-- useless by itself. helper for categorical'
pick :: [(Double, a)] -> Double -> Double -> (Double, a)
pick [] s i = error "can't pick from empty list"
pick xs s i = let (w, x) = go i 0 xs in (log w - log s, x)
  where
    go i a (x:[]) = x
    go i a (x:xs)
      | i <= (a + fst x) = x
      | otherwise        = go i (a + fst x) xs

-- Choose randomly from an arbitrarily weighted list
categorical :: MonadRandom m => [(Double, a)] -> P m a
categorical ws = sampleP (P r)
  where s = sum (map fst ws)
        r = pick ws s <$> uniformT 0 s
