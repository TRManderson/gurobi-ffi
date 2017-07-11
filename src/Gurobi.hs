module Gurobi
    ( someFunc
    ) where

import Gurobi.Internal (x)

someFunc :: IO ()
someFunc = putStrLn $ "someFunc" ++ show x
