{-# LANGUAGE ForeignFunctionInterface, NoImplicitPrelude, TemplateHaskell #-}
module Numeric.Gurobi.C where
import BasicPrelude
import Foreign.C
import Foreign

--Environment Creation and Destruction

data Env = Env
type EnvHandle = Ptr Env

data Model = Model
type ModelHandle = Ptr Model

-- loadenv
foreign import ccall "GRBloadenv" loadEnv :: Ptr EnvHandle -> CString -> IO CInt

-- loadclientenv
foreign import ccall "GRBloadclientenv" loadClientEnv :: Ptr EnvHandle -> CString -> CString -> CInt -> CString -> CInt -> CDouble

-- freeenv
foreign import ccall "GRBfreeenv" freeEnv :: EnvHandle -> IO ()

-- getconcurrentenv
foreign import ccall "GRBgetconcurrentenv" getConcurrentEnv :: ModelHandle -> CInt -> IO EnvHandle

-- discardconcurrentenvs
foreign import ccall "GRBdiscardconcurrentenvs" discardConcurrentEnvs :: ModelHandle -> IO ()

--Model Creation and Modification

-- loadmodel
foreign import ccall "GRBloadmodel" loadModel :: EnvHandle -> Ptr ModelHandle -> CString -> CInt -> CInt -> CInt -> CDouble -> Ptr CDouble -> Ptr CChar -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr CString -> Ptr CString -> IO CInt

-- Xloadmodel
-- foreign import ccall "GRBXloadmodel" loadModelX :: IO ()

-- newmodel
foreign import ccall "GRBnewmodel" newModel :: EnvHandle -> Ptr ModelHandle -> CString -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr CString -> IO CInt

-- copymodel
foreign import ccall "GRBcopymodel" copyModel :: ModelHandle -> IO ModelHandle

-- addconstr
foreign import ccall "GRBaddconstr" addConstr :: ModelHandle -> CInt -> Ptr CInt -> Ptr CDouble -> CChar -> CDouble -> CString -> IO CInt

-- addconstrs
foreign import ccall "GRBaddconstrs" addConstrs :: ModelHandle -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CChar -> Ptr CDouble -> Ptr CString

-- Xaddconstrs
-- foreign import ccall "GRBXaddconstrs" addConstrsX :: IO ()

-- addqconstr
-- foreign import ccall "GRBaddqconstr" addQConstr :: IO ()

-- addqpterms
-- foreign import ccall "GRBaddqpterms" addQPTerms :: IO ()

-- addrangeconstr
foreign import ccall "GRBaddrangeconstr" addRangeConstr :: ModelHandle -> CInt -> Ptr CInt -> Ptr CDouble -> CDouble -> CDouble -> CString -> IO CInt

-- addrangeconstrs
foreign import ccall "GRBaddrangeconstrs" addRangeConstrs :: ModelHandle -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CString -> IO CInt

-- Xaddrangeconstrs
-- foreign import ccall "GRBXaddrangeconstrs" addRangeConstrsX :: IO ()

-- addsos
foreign import ccall "GRBaddsos" addSOS :: ModelHandle -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- addvar
foreign import ccall "GRBaddvar" addVar :: ModelHandle -> CInt -> Ptr CInt -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> CChar -> CString -> IO CInt

-- addvars
foreign import ccall "GRBaddvars" addVars :: ModelHandle -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> Ptr CString -> IO CInt

-- Xaddvars
-- foreign import ccall "GRBxaddvars" addVarsX :: IO ()

-- chgcoeffs
-- foreign import ccall "GRBchgcoeffs" chgCoeffs :: IO ()

-- Xchgcoeffs
-- foreign import ccall "GRBXchgcoeffs" chgcoeffsX :: IO ()

-- delconstrs
-- foreign import ccall "GRBdelconstrs" delconstrs :: IO ()

-- delq
-- foreign import ccall "GRBdelq" delq :: IO ()

-- delqconstrs
-- foreign import ccall "GRBdelqconstrs" delqconstrs :: IO ()

-- delsos
-- foreign import ccall "GRBdelsos" delsos :: IO ()

-- delvars
-- foreign import ccall "GRBdelvars" delvars :: IO ()

-- setpwlobj
-- foreign import ccall "GRBsetpwlobj" setpwlobj :: IO ()

-- updatemodel
-- foreign import ccall "GRBupdatemodel" updatemodel :: IO ()

-- freemodel
-- foreign import ccall "GRBfreemodel" freemodel :: IO ()


--Model Solution

-- optimize
-- foreign import ccall "GRBoptimize" optimize :: IO ()

-- optimizeasync
-- foreign import ccall "GRBoptimizeasync" optimizeasync :: IO ()

-- computeIIS
-- foreign import ccall "GRBcomputeiis" computeIIS :: IO ()

-- feasrelax
-- foreign import ccall "GRBfeasrelax" feasrelax :: IO ()

-- fixedmodel
-- foreign import ccall "GRBfixedmodel" fixedmodel :: IO ()

-- resetmodel
-- foreign import ccall "GRBresetmodel" resetmodel :: IO ()

-- sync
-- foreign import ccall "GRBsync" sync :: IO ()

--Model Queries

-- getcoeff
-- foreign import ccall "GRBgetcoeff" getcoeff :: IO ()

-- getconstrbyname
-- foreign import ccall "GRBgetconstrbyname" getconstrbyname :: IO ()

-- getconstrs
-- foreign import ccall "GRBgetconstrs" getconstrs :: IO ()

-- Xgetconstrs
-- foreign import ccall "GRBXgetconstrs" getconstrs_st :: IO ()

-- getenv
-- foreign import ccall "GRBgetenv" getenv :: IO ()

-- getpwlobj
-- foreign import ccall "GRBgetpwlobj" getpwlobj :: IO ()

-- getq
-- foreign import ccall "GRBgetq" getq :: IO ()

-- getqconstr
-- foreign import ccall "GRBgetqconstr" getqconstr :: IO ()

-- getsos
-- foreign import ccall "GRBgetsos" getsos :: IO ()

-- getvarbyname
-- foreign import ccall "GRBgetvarbyname" getvarbyname :: IO ()

-- getvars
-- foreign import ccall "GRBgetvars" getvars :: IO ()

-- Xgetvars
-- foreign import ccall "GRBXgetvars" getvars_st :: IO ()

--Input/Output

-- readmodel
-- foreign import ccall "GRBreadmodel" readmodel :: IO ()

-- read
-- foreign import ccall "GRBread" read :: IO ()

-- write
-- foreign import ccall "GRBwrite" write :: IO ()

--Attribute Management

-- getattrinfo
-- foreign import ccall "GRBgetattrinfo" getattrinfo :: IO ()

-- getintattr
-- foreign import ccall "GRBgetintattr" getintattr :: IO ()

-- setintattr
-- foreign import ccall "GRBsetintattr" setintattr :: IO ()

-- getintattrelement
-- foreign import ccall "GRBgetintattrelement" getintattrelement :: IO ()

-- setintattrelement
-- foreign import ccall "GRBsetintattrelement" setintattrelement :: IO ()

-- getintattrarray
-- foreign import ccall "GRBgetintattrarray" getintattrarray :: IO ()

-- setintattrarray
-- foreign import ccall "GRBsetintattrarray" setintattrarray :: IO ()

-- getintattrlist
-- foreign import ccall "GRBgetintattrlist" getintattrlist :: IO ()

-- setintattrlist
-- foreign import ccall "GRBsetintattrlist" setintattrlist :: IO ()

-- getdblattr
-- foreign import ccall "GRBgetdblattr" getdblattr :: IO ()

-- setdblattr
-- foreign import ccall "GRBsetdblattr" setdblattr :: IO ()

-- getdblattrelement
-- foreign import ccall "GRBgetdblattrelement" getdblattrelement :: IO ()

-- setdblattrelement
-- foreign import ccall "GRBsetdblattrelement" setdblattrelement :: IO ()

-- getdblattrarray
-- foreign import ccall "GRBgetdblattrarray" getdblattrarray :: IO ()

-- setdblattrarray
-- foreign import ccall "GRBsetdblattrarray" setdblattrarray :: IO ()

-- getdblattrlist
-- foreign import ccall "GRBgetdblattrlist" getdblattrlist :: IO ()

-- setdblattrlist
-- foreign import ccall "GRBsetdblattrlist" setdblattrlist :: IO ()

-- getcharattrelement
-- foreign import ccall "GRBgetcharattrelement" getcharattrelement :: IO ()

-- setcharattrelement
-- foreign import ccall "GRBsetcharattrelement" setcharattrelement :: IO ()

-- getcharattrarray
-- foreign import ccall "GRBgetcharattrarray" getcharattrarray :: IO ()

-- setcharattrarray
-- foreign import ccall "GRBsetcharattrarray" setcharattrarray :: IO ()

-- getcharattrlist
-- foreign import ccall "GRBgetcharattrlist" getcharattrlist :: IO ()

-- setcharattrlist
-- foreign import ccall "GRBsetcharattrlist" setcharattrlist :: IO ()

-- getstrattr
-- foreign import ccall "GRBgetstrattr" getstrattr :: IO ()

-- setstrattr
-- foreign import ccall "GRBsetstrattr" setstrattr :: IO ()

-- getstrattrelement
-- foreign import ccall "GRBgetstrattrelement" getstrattrelement :: IO ()

-- setstrattrelement
-- foreign import ccall "GRBsetstrattrelement" setstrattrelement :: IO ()

-- getstrattrarray
-- foreign import ccall "GRBgetstrattrarray" getstrattrarray :: IO ()

-- setstrattrarray
-- foreign import ccall "GRBsetstrattrarray" setstrattrarray :: IO ()

-- getstrattrlist
-- foreign import ccall "GRBgetstrattrlist" getstrattrlist :: IO ()

-- setstrattrlist
-- foreign import ccall "GRBsetstrattrlist" setstrattrlist :: IO ()

--Parameter Management and Tuning

-- tunemodel
-- foreign import ccall "GRBtunemodel" tunemodel :: IO ()

-- gettuneresult
-- foreign import ccall "GRBgettuneresult" gettuneresult :: IO ()

-- getdblparam
-- foreign import ccall "GRBgetdblparam" getdblparam :: IO ()

-- getintparam
-- foreign import ccall "GRBgetintparam" getintparam :: IO ()

-- getstrparam
-- foreign import ccall "GRBgetstrparam" getstrparam :: IO ()

-- setdblparam
-- foreign import ccall "GRBsetdblparam" setdblparam :: IO ()

-- setintparam
-- foreign import ccall "GRBsetintparam" setintparam :: IO ()

-- setstrparam
-- foreign import ccall "GRBsetstrparam" setstrparam :: IO ()

-- getdblparaminfo
-- foreign import ccall "GRBgetdblparaminfo" getdblparaminfo :: IO ()

-- getintparaminfo
-- foreign import ccall "GRBgetintparaminfo" getintparaminfo :: IO ()

-- getstrparaminfo
-- foreign import ccall "GRBgetstrparaminfo" getstrparaminfo :: IO ()

-- readparams
-- foreign import ccall "GRBreadparams" readparams :: IO ()

-- writeparams
-- foreign import ccall "GRBwriteparams" writeparams :: IO ()

--Monitoring Progress - Logging and Callbacks

-- msg
-- foreign import ccall "GRBmsg" msg :: IO ()

-- setcallbackfunc
-- foreign import ccall "GRBsetcallbackfunc" setcallbackfunc :: IO ()

-- getcallbackfunc
-- foreign import ccall "GRBgetcallbackfunc" getcallbackfunc :: IO ()

-- cbget
-- foreign import ccall "GRBcbget" cbget :: IO ()

-- version
-- foreign import ccall "GRBversion" version :: IO ()

--Modifying Solver Behavior - Callbacks

-- cbcut
-- foreign import ccall "GRBcbcut" cbcut :: IO ()

-- cblazy
-- foreign import ccall "GRBcblazy" cblazy :: IO ()

-- cbsolution
-- foreign import ccall "GRBcbsolution" cbsolution :: IO ()

-- terminate
-- foreign import ccall "GRBterminate" terminate :: IO ()

--Error Handling

-- geterrormsg
-- foreign import ccall "GRBgeterrormsg" geterrormsg :: IO ()

--Advanced simplex routines

-- FSolve
-- foreign import ccall "GRBFSolve" fSolve :: IO ()

-- BSolve
-- foreign import ccall "GRBBSolve" bSolve :: IO ()

-- BinvColj
-- foreign import ccall "GRBBinvcolj" bInvColj :: IO ()

-- BinvRowi
-- foreign import ccall "GRBBinvrowi" bInvRowi :: IO ()

-- getBasisHead
-- foreign import ccall "GRBgetBasisHead" getBasisHead :: IO ()