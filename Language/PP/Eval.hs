module Language.PP.Eval where

import Control.Monad
import Control.Monad.Free

import Language.PP.Types

-- | Declare an observation with weight 'p'
observe :: Monad m => Double -> PP m ()
observe p = liftF . P . return $ (p, ())

-- | Generate data + probability from the model
eval :: Monad m => PP m a -> m (Probability, a)
eval prog = go 0 prog
  where
    go p (Pure x)     = return (p, x)
    go p (Free (P m)) = m >>= (\(p', x) -> go (p + p') x)

-- | Generate data from the model using the IO instance of MonadRandom
genIO :: PP IO Double -> IO Double
genIO (Pure x)     = return x
genIO (Free (P m)) = m >>= (genIO . snd)

