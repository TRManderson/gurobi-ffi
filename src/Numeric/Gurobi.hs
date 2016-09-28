module Lib
    ( loadEnv
    ) where

import Numeric.Gurobi.C as C
import Numeric.Gurobi



loadEnv :: String -> IO Env
loadEnv logfilename = undefined