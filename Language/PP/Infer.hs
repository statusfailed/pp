module Language.PP.Infer where

import Control.Monad
import Control.Monad.Free
import Language.PP.Dist
import Language.PP.Types
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

getPure :: PP m a -> a
getPure (Pure x) = x
getPure (Free x) = error "don't call getPure on Free!"

-- | The bootstrap filter is a distribution over probabilistic programs
bootstrap :: MonadRandom m => Int -> PP m a -> PP m [a]
bootstrap n prog = loopstrap n (replicate n prog)

-- | Run all interpreters and resample the "fittest" ones
loopstrap :: MonadRandom m => Int -> [PP m a] -> PP m [a]
loopstrap n ps
  | all terminated ps = return $ map getPure ps
  | otherwise = replicateM n (liftF $ resample ps) >>= loopstrap n
