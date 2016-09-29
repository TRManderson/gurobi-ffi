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


data ModelAttributes = NumConstrs
                     | NumVars
                     | NumSOS
                     | NumQConstrs
                     | NumNZs
                     | DNumNZs
                     | NumQNZs
                     | NumQCNZs
                     | NumIntVars
                     | NumBinVars
                     | NumPWLObjVars
                     | ModelName
                     | ModelSense
                     | ObjCon
                     | IsMIP
                     | IsQP
                     | IsQCP
                     | Server
                     deriving (Show, Read)

data VariableAttributes
data ConstraintAttributes

data Variable = Variable { objectiveCoefficient :: Double
                         , lowerBound :: Double
                         , upperBound :: Double
                         , vType :: VariableType
                         , vName :: String
                         }

unVar :: Variable -> (Double, Double, Double, VariableType, String)
unVar (Variable a b c d e) = (a, b, c, d, e)
unVars :: [Variable] -> (Int, [Double], [Double], [Double], [VariableType], [String])
unVars [] = (0, [], [], [], [], [])
unVars (x:xs) = (a+1, p:b, q:c, r:d, s:e, t:f)
  where (a, b, c, d, e, f) = unVars xs
        (p, q, r, s, t) = unVar x
