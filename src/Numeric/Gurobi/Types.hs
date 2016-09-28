{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Gurobi.Types where
import BasicPrelude
import Control.Monad.Trans.Except

data VariableType = Continuous | Binary | Integer | SemiContinuous | SemiInteger
data ObjectiveSense = Minimize | Maximize
type ConstraintSense = Ordering
