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
foreign import ccall "GRBupdatemodel" updateModel :: ModelHandle -> IO CInt

-- freemodel
foreign import ccall "GRBfreemodel" freeModel :: ModelHandle -> IO CInt


--Model Solution

-- optimize
foreign import ccall "GRBoptimize" optimize :: ModelHandle -> IO ()

-- optimizeasync
foreign import ccall "GRBoptimizeasync" optimizeAsync :: ModelHandle -> IO ()

-- computeIIS
-- foreign import ccall "GRBcomputeiis" computeIIS :: IO ()

-- feasrelax
-- foreign import ccall "GRBfeasrelax" feasRelax :: IO ()

-- fixedmodel
foreign import ccall "GRBfixedmodel" fixedModel :: ModelHandle -> IO ModelHandle

-- resetmodel
foreign import ccall "GRBresetmodel" resetModel :: ModelHandle -> IO ()

-- sync
foreign import ccall "GRBsync" sync :: ModelHandle -> IO ()

--Model Queries

-- getcoeff
foreign import ccall "GRBgetcoeff" getCoeff :: ModelHandle -> CInt -> CInt -> Ptr CDouble -> IO CInt

-- getconstrbyname
foreign import ccall "GRBgetconstrbyname" getConstrByName :: ModelHandle -> CString -> Ptr CInt -> IO CInt

-- getconstrs
foreign import ccall "GRBgetconstrs" getConstrs :: ModelHandle -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt

-- Xgetconstrs
-- foreign import ccall "GRBXgetconstrs" getconstrs_st :: IO ()

-- getenv
foreign import ccall "GRBgetenv" getEnv :: ModelHandle -> IO EnvHandle

-- getpwlobj
-- foreign import ccall "GRBgetpwlobj" getpwlobj :: IO ()

-- getq
-- foreign import ccall "GRBgetq" getq :: IO ()

-- getqconstr
-- foreign import ccall "GRBgetqconstr" getqconstr :: IO ()

-- getsos
-- foreign import ccall "GRBgetsos" getsos :: IO ()

-- getvarbyname
foreign import ccall "GRBgetvarbyname" getVarByName :: ModelHandle -> CString -> Ptr CInt -> IO CInt

-- getvars
foreign import ccall "GRBgetvars" getVars :: ModelHandle -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> CInt -> CInt -> IO CInt

-- Xgetvars
-- foreign import ccall "GRBXgetvars" getvars_st :: IO ()

--Input/Output

-- readmodel
foreign import ccall "GRBreadmodel" readModel :: EnvHandle -> CString -> Ptr ModelHandle -> IO CInt

-- read
foreign import ccall "GRBread" read :: ModelHandle -> CString -> IO CInt

-- write
foreign import ccall "GRBwrite" write :: ModelHandle -> CString -> IO CInt

--Attribute Management

-- getattrinfo
foreign import ccall "GRBgetattrinfo" getAttrInfo :: ModelHandle -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- getintattr
foreign import ccall "GRBgetintattr" getIntAttr :: ModelHandle -> CString -> Ptr CInt -> IO CInt

-- setintattr
foreign import ccall "GRBsetintattr" setIntAttr :: ModelHandle -> CString -> CInt -> IO CInt

-- getintattrelement
foreign import ccall "GRBgetintattrelement" getIntAttrElement :: ModelHandle -> CString -> CInt -> CInt -> Ptr CInt -> IO CInt

-- setintattrelement
foreign import ccall "GRBsetintattrelement" setintattrElement :: ModelHandle -> CString -> CInt -> CInt -> IO CInt

-- getintattrarray
foreign import ccall "GRBgetintattrarray" getIntAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CInt -> IO CInt

-- setintattrarray
foreign import ccall "GRBsetintattrarray" setIntAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CInt -> IO CInt

-- getintattrlist
foreign import ccall "GRBgetintattrlist" getIntAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- setintattrlist
foreign import ccall "GRBsetintattrlist" setIntAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- getdblattr
foreign import ccall "GRBgetdblattr" getDblAttr :: ModelHandle -> CString -> Ptr CDouble -> IO CInt

-- setdblattr
foreign import ccall "GRBsetdblattr" setDblAttr :: ModelHandle -> CString -> CDouble -> IO CInt

-- getdblattrelement
foreign import ccall "GRBgetdblattrelement" getDblAttrElement :: ModelHandle -> CString -> CInt -> CInt -> Ptr CDouble -> IO CInt

-- setdblattrelement
foreign import ccall "GRBsetdblattrelement" setDblattrElement :: ModelHandle -> CString -> CInt -> CDouble -> IO CInt

-- getdblattrarray
foreign import ccall "GRBgetdblattrarray" getDblAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CDouble -> IO CInt

-- setdblattrarray
foreign import ccall "GRBsetdblattrarray" setDblAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CDouble -> IO CInt

-- getdblattrlist
foreign import ccall "GRBgetdblattrlist" getDblAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- setdblattrlist
foreign import ccall "GRBsetdblattrlist" setDblAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- getcharattrelement
foreign import ccall "GRBgetcharattrelement" getCharAttrElement :: ModelHandle -> CString -> CInt -> CInt -> Ptr CChar -> IO CInt

-- setcharattrelement
foreign import ccall "GRBsetcharattrelement" setCharattrElement :: ModelHandle -> CString -> CInt -> CChar -> IO CInt

-- getcharattrarray
foreign import ccall "GRBgetcharattrarray" getCharAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CChar -> IO CInt

-- setcharattrarray
foreign import ccall "GRBsetcharattrarray" setCharAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CChar -> IO CInt

-- getcharattrlist
foreign import ccall "GRBgetcharattrlist" getCharAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CChar -> IO CInt

-- setcharattrlist
foreign import ccall "GRBsetcharattrlist" setCharAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CChar -> IO CInt

-- getstrattrelement
foreign import ccall "GRBgetstrattrelement" getStrAttrElement :: ModelHandle -> CString -> CInt -> CInt -> Ptr CString -> IO CInt

-- setstrattrelement
foreign import ccall "GRBsetstrattrelement" setStrAttrElement :: ModelHandle -> CString -> CInt -> CString -> IO CInt

-- getstrattrarray
foreign import ccall "GRBgetstrattrarray" getStrAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CString -> IO CInt

-- setstrattrarray
foreign import ccall "GRBsetstrattrarray" setStrAttrArray :: ModelHandle -> CString -> CInt -> CInt -> Ptr CString -> IO CInt

-- getstrattrlist
foreign import ccall "GRBgetstrattrlist" getStrAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CString -> IO CInt

-- setstrattrlist
foreign import ccall "GRBsetstrattrlist" setStrAttrList :: ModelHandle -> CString -> CInt -> Ptr CInt -> Ptr CString -> IO CInt


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