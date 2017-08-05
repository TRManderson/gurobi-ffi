{-# LANGUAGE CPP, ForeignFunctionInterface, GADTs, MultiParamTypeClasses #-}

module Gurobi.Internal where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Concurrent.MVar
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

#let STORABLE hstype,ctype,toC,fromC = "\
instance Storable (" hstype ") where\n\
  sizeOf _ = sizeOf (undefined :: " ctype ")\n\
  alignment _ = alignment (undefined :: " ctype ")\n\
  peek p = fmap (" fromC ") . peek $ (castPtr p :: Ptr (" ctype "))\n\
  poke p v = poke (castPtr p) ((" toC " $ v) :: " ctype " )"

#include <gurobi_c.h>

data GurobiModel
data GurobiEnv

type GurobiModelP = Ptr GurobiModel
type GurobiEnvP = Ptr GurobiEnv

version = ( #const GRB_VERSION_MAJOR
          , #const GRB_VERSION_MINOR
          , #const GRB_VERSION_TECHNICAL
          )
defaultServerPriority = #const DEFAULT_CS_PRIORITY
maxComputeServerPriority = #const MAX_CS_PRIORITY
defaultComputeServerPort = #const DEFAULT_CS_PORT

data GurobiError = OutOfMemory
                 | NullArgument
                 | InvalidArgument
                 | UnknownAttribute
                 | DataNotAvailable
                 | IndexOutOfRange
                 | UnknownParameter
                 | ValueOutOfRange
                 | NoLicense
                 | SizeLimitExceeded
                 | Callback
                 | FileRead
                 | FileWrite
                 | Numeric
                 | IisNotInfeasible
                 | NotForMIP
                 | OptimizationInProgress
                 | Duplicates
                 | Nodefile
                 | QNotPsd
                 | QCPEqualityConstraint
                 | Network
                 | JobRejected
                 | NotSupported
                 | Exceed2BNonzeros
                 | InvalidPiecewiseObj
                 | UpdatemodeChange
                 | Cloud
                 | ModelModification
                 | NotInModel
                 | FailedToCreateModel
                 | Internal
                 | OtherError CInt

instance Bounded GurobiError where
    minBound = OutOfMemory
    maxBound = Internal

instance Enum GurobiError where
  fromEnum OutOfMemory = 10001
  fromEnum NullArgument = 10002
  fromEnum InvalidArgument = 10003
  fromEnum UnknownAttribute = 10004
  fromEnum DataNotAvailable = 10005
  fromEnum IndexOutOfRange = 10006
  fromEnum UnknownParameter = 10007
  fromEnum ValueOutOfRange = 10008
  fromEnum NoLicense = 10009
  fromEnum SizeLimitExceeded = 10010
  fromEnum Callback = 10011
  fromEnum FileRead = 10012
  fromEnum FileWrite = 10013
  fromEnum Numeric = 10014
  fromEnum IisNotInfeasible = 10015
  fromEnum NotForMIP = 10016
  fromEnum OptimizationInProgress = 10017
  fromEnum Duplicates = 10018
  fromEnum Nodefile = 10019
  fromEnum QNotPsd = 10020
  fromEnum QCPEqualityConstraint = 10021
  fromEnum Network = 10022
  fromEnum JobRejected = 10023
  fromEnum NotSupported = 10024
  fromEnum Exceed2BNonzeros = 10025
  fromEnum InvalidPiecewiseObj = 10026
  fromEnum UpdatemodeChange = 10027
  fromEnum Cloud = 10028
  fromEnum ModelModification = 10029
  fromEnum NotInModel = 20001
  fromEnum FailedToCreateModel = 20002
  fromEnum Internal = 20003
  fromEnum (OtherError x) = fromEnum x
  toEnum 10001 = OutOfMemory
  toEnum 10002 = NullArgument
  toEnum 10003 = InvalidArgument
  toEnum 10004 = UnknownAttribute
  toEnum 10005 = DataNotAvailable
  toEnum 10006 = IndexOutOfRange
  toEnum 10007 = UnknownParameter
  toEnum 10008 = ValueOutOfRange
  toEnum 10009 = NoLicense
  toEnum 10010 = SizeLimitExceeded
  toEnum 10011 = Callback
  toEnum 10012 = FileRead
  toEnum 10013 = FileWrite
  toEnum 10014 = Numeric
  toEnum 10015 = IisNotInfeasible
  toEnum 10016 = NotForMIP
  toEnum 10017 = OptimizationInProgress
  toEnum 10018 = Duplicates
  toEnum 10019 = Nodefile
  toEnum 10020 = QNotPsd
  toEnum 10021 = QCPEqualityConstraint
  toEnum 10022 = Network
  toEnum 10023 = JobRejected
  toEnum 10024 = NotSupported
  toEnum 10025 = Exceed2BNonzeros
  toEnum 10026 = InvalidPiecewiseObj
  toEnum 10027 = UpdatemodeChange
  toEnum 10028 = Cloud
  toEnum 10029 = ModelModification
  toEnum 20001 = NotInModel
  toEnum 20002 = FailedToCreateModel
  toEnum 20003 = Internal
  toEnum x = OtherError $ toEnum x

#STORABLE "GurobiError","CInt","toEnum.fromEnum","toEnum.fromEnum"

newtype ConstraintSense = CS Ordering

#STORABLE "ConstraintSense","CChar","castCharToCChar.csToChar","charToCS.castCCharToChar"

csToChar :: ConstraintSense -> Char
csToChar (CS GT) = '>'
csToChar (CS LT) = '<'
csToChar (CS EQ) = '='

charToCS :: Char -> ConstraintSense
charToCS '>' = CS GT
charToCS '<' = CS LT
charToCS '=' = CS EQ
charToCS c   = error ("Bad ConstraintSense: '" ++ c:"' is not valid")

data VariableType = Continuous
                  | Binary
                  | Integer
                  | SemiContinuous
                  | SemiInt
varTypeToChar :: VariableType -> Char
varTypeToChar Continuous = 'C'
varTypeToChar Binary = 'B'
varTypeToChar Integer = 'I'
varTypeToChar SemiContinuous = 'S'
varTypeToChar SemiInt = 'N'

charToVarType :: Char -> VariableType
charToVarType 'C' = Continuous
charToVarType 'B' = Binary
charToVarType 'I' = Integer
charToVarType 'S' = SemiContinuous
charToVarType 'N' = SemiInt
charToVarType c = error ("Bad VariableType: '" ++ c:"' is not valid")

#STORABLE "VariableType","CChar","(castCharToCChar.varTypeToChar)","(charToVarType.castCCharToChar)"

data ObjectiveSense = Minimize | Maximize
osToInt :: ObjectiveSense -> Int
osToInt Minimize = 1
osToInt Maximize = -1

intToOS :: Int -> ObjectiveSense
intToOS 1 = Minimize
intToOS (-1) = Maximize
intToOS v = error ("Bad VariableType: " ++ show v ++ " is not valid")

#STORABLE "ObjectiveSense","CInt","(toEnum.osToInt)","(intToOS.fromEnum)"

grbInfinity = #const GRB_INFINITY
grbUndefined = #const GRB_UNDEFINED
grbMaxInt = #const GRB_MAXINT

grbMaxNamelen = #const GRB_MAX_NAMELEN
grbMaxStrlen = #const GRB_MAX_STRLEN
grbMaxConcurrent = #const GRB_MAX_CONCURRENT

data AttributeType = ModelAttr
                   | VariableAttr
                   | LinearConstraintAttr
                   | SOSConstraintAttr
                   | QuadraticConstraintAttr
                   | GeneralConstraintAttr

instance Bounded AttributeType where
  minBound = ModelAttr
  maxBound = GeneralConstraintAttr

instance Enum AttributeType where
  fromEnum ModelAttr = 0
  fromEnum VariableAttr = 1
  fromEnum LinearConstraintAttr = 2
  fromEnum SOSConstraintAttr = 3
  fromEnum QuadraticConstraintAttr = 4
  fromEnum GeneralConstraintAttr = 5
  toEnum 0 = ModelAttr
  toEnum 1 = VariableAttr
  toEnum 2 = LinearConstraintAttr
  toEnum 3 = SOSConstraintAttr
  toEnum 4 = QuadraticConstraintAttr
  toEnum 5 = GeneralConstraintAttr

#STORABLE "AttributeType","CInt","(toEnum.fromEnum)","(toEnum.fromEnum)"

data AttributeDataType = CharAttr
                       | IntAttr
                       | DoubleAttr
                       | StringAttr

-- class GurobiAttributeDataType a where
--   toAttributeDataType :: a -> AttributeDataType
-- instance GurobiAttributeDataType Char where
--   toAttributeDataType _ = CharAttr
-- instance GurobiAttributeDataType Int where
--   toAttributeDataType _ = IntAttr
-- instance GurobiAttributeDataType Double where
--   toAttributeDataType _ = DoubleAttr
-- instance GurobiAttributeDataType String where
--   toAttributeDataType _ = StringAttr

instance Bounded AttributeDataType where
  minBound = CharAttr
  maxBound = StringAttr

instance Enum AttributeDataType where
  fromEnum CharAttr = 0
  fromEnum IntAttr = 1
  fromEnum DoubleAttr = 2
  fromEnum StringAttr = 3
  toEnum 0 = CharAttr
  toEnum 1 = IntAttr
  toEnum 2 = DoubleAttr
  toEnum 3 = StringAttr

#STORABLE "AttributeDataType","CInt","(toEnum.fromEnum)","(toEnum.fromEnum)"

foreign import ccall "GRBgetattrinfo" c_getAttrInfo :: GurobiModelP -> CString -> Ptr AttributeDataType -> Ptr AttributeType -> Ptr Bool -> IO CInt

foreign import ccall "GRBisattravailable" c_isAttrAvailable :: GurobiModelP -> CString -> IO CInt


grbErrorReturn :: CInt -> (GurobiError -> a) -> (a) -> a
grbErrorReturn x f s = case x of
  0 -> s
  x' -> f.toEnum.fromEnum $ x'

foreign import ccall "GRBloadenv" c_loadEnv :: Ptr GurobiEnvP -> CString -> IO CInt
loadEnv :: String -> ExceptT GurobiError IO GurobiEnvP
loadEnv logFileName = ExceptT . alloca $ \ptr -> do
  retval <- withCString logFileName (c_loadEnv ptr)
  grbErrorReturn retval (pure.Left) (Right <$> peek ptr)


foreign import ccall "GRBnewmodel" c_newModel :: GurobiEnvP -> Ptr GurobiModelP -> IO CInt
newModel :: GurobiEnvP -> ExceptT GurobiError IO GurobiModelP
newModel envP = ExceptT . alloca $ \ptr -> do
  retval <- c_newModel envP ptr
  grbErrorReturn retval (pure.Left) (Right <$> peek ptr)

foreign import ccall "GRBaddvar" c_addVar :: GurobiModelP -> Int -> Ptr Int -> Ptr Double -> Double -> Double -> Double -> Char -> CString -> CInt
-- addVar :: Model -> Variable ->

newtype Model = Model { unModel :: MVar ModelData }
newtype Variable = Variable { unVariable :: MVar VariableData }

type GurobiM a = ExceptT GurobiError IO a

addVar :: Model -> Variable -> GurobiM ()
addVar (Model mref) (Variable vref) = do
  (ModelData mp cc vc) <- mref
  (VariableData lb ub vt n r)
  let vtc = castCharToCChar . varTypeToChar $ vt
  retval <- withCString n (\cname -> c_addVar mp 0 nullPtr nullPtr 0 lb ub vt vtc cname)
  grbErrorReturn retval throwE (Right ())


class Attrable a t where
    getAttr :: a -> ExceptT GurobiError IO t
    setAttr :: a -> t -> ExceptT GurobiError IO ()


data VariableData = VariableData { lowerBound :: Double
                                 , upperBound :: Double
                                 , vtype :: VariableType
                                 , name :: String
                                 , ref :: Maybe Int
                                 }


data ModelData = ModelData { modelPtr :: GurobiModelP
                       , constraintCounter :: Int
                       , variableCounter :: Int
                       }


-- foreign import ccall "GRBaddconstr" c_addConstr

