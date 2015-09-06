module Main where

import Control.Monad.Free
import Language.PP
import Language.PP.Types
import Language.PP.Dist
import Language.PP.Eval
import Language.PP.Infer
import qualified Language.PP.Examples.StickyState as SSM

main = SSM.main
