module Main where

import Control.Monad.Free
import Language.PP
import Language.PP.Dist
import Language.PP.Eval
import Language.PP.Infer
import Data.Random (MonadRandom)

modelPmmh :: MonadRandom m => PMMH m Double [S]
modelPmmh = (liftF (uniform 0 1), (\x y -> 1), model)

main = eval (pmmh 1000 10 modelPmmh) >>= mapM_ print . snd
