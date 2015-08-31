module Language.PP where

import Control.Monad
import Control.Monad.Free
import Language.PP.Types
import Language.PP.Dist
import Language.PP.Infer

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
