{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Gurobi.Types where
import BasicPrelude
import Numeric.Gurobi.C (ModelHandle, EnvHandle)
import Control.Monad.Trans.Except


newtype Model = Model { _unModel :: ModelHandle}
newtype Env = Env { _unEnv :: EnvHandle}

data GurobiError = OutOfMemoryError
                 | NullArgumentError
                 | InvalidArgumentError
                 | UnknownAttributeError
                 | DataNotAvailableError
                 | IndexOutOfRangeError
                 | UnknownParameterError
                 | ValueOutOfRangeError
                 | NoLicenseError
                 | SizeLimitExceededError
                 | CallbackError
                 | FileReadError
                 | FileWriteError
                 | NumericError
                 | IISNotInfeasibleError
                 | NotForMIPError
                 | OptimizationInProgressError
                 | DuplicatesError
                 | NodeFileError
                 | QNotPSDError
                 | QCPEqualityConstraintError
                 | NetworkError
                 | JobRejectedError
                 | NotSupportedError
                 | Exceed2BNonZerosError
                 | InvalidPiecewiseObjError
                 | NotInModelError
                 | FailedToCreateModelError
                 | GurobiInternalError
                 | NotImplementedError


type Gurobi a = ExceptT GurobiError IO a


data VariableType = Continuous | Binary | Integer | SemiContinuous | SemiInteger
data ObjectiveSense = Minimize | Maximize
type ConstraintSense = Ordering
