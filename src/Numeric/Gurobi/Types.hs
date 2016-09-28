{-# LANGUAGE FunctionalDependencies #-}
module Numeric.Gurobi.Types where

type Env = ()


data VariableType = Continuous | Binary | Integer | SemiContinuous | SemiInteger
data ObjectiveSense = Minimize | Maximize
type ConstraintSense = Ordering
