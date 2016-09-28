{-# LANGUAGE ForeignFunctionInterface #-}
module Numeric.Gurobi.C where

import Foreign.C

data Model = Model

foreign import ccall "name_of_function" function
 :: Ptr a -> IO (Ptr a)


foreign import ccall "GRBgetattrinfo" getAttrInfo :: Ptr (Model) :: CString -> CIntPtr :: CIntPtr -> CIntPtr -> IO CInt

foreign import ccall "GRBisattravailable"

{-
foreign import ccall int GRBisattravailable(GRBmodel *model, const char *attrname);
foreign import ccall int GRBgetintattr(GRBmodel *model, const char *attrname, int *valueP);
foreign import ccall int GRBsetintattr(GRBmodel *model, const char *attrname, int newvalue);
foreign import ccall int GRBgetintattrelement(GRBmodel *model, const char *attrname,
                       int element, int *valueP);
foreign import ccall int GRBsetintattrelement(GRBmodel *model, const char *attrname,
                       int element, int newvalue);
foreign import ccall int GRBgetintattrarray(GRBmodel *model, const char *attrname,
                     int first, int len, int *values);
foreign import ccall int GRBsetintattrarray(GRBmodel *model, const char *attrname,
                     int first, int len, int *newvalues);
foreign import ccall int GRBgetintattrlist(GRBmodel *model, const char *attrname,
                    int len, int *ind, int *values);
foreign import ccall int GRBsetintattrlist(GRBmodel *model, const char *attrname,
                    int len, int *ind, int *newvalues);
foreign import ccall int GRBgetcharattrelement(GRBmodel *model, const char *attrname,
                        int element, char *valueP);
foreign import ccall int GRBsetcharattrelement(GRBmodel *model, const char *attrname,
                        int element, char newvalue);
foreign import ccall int GRBgetcharattrarray(GRBmodel *model, const char *attrname,
                      int first, int len, char *values);
foreign import ccall int GRBsetcharattrarray(GRBmodel *model, const char *attrname,
                      int first, int len, char *newvalues);
foreign import ccall int GRBgetcharattrlist(GRBmodel *model, const char *attrname,
                     int len, int *ind, char *values);
foreign import ccall int GRBsetcharattrlist(GRBmodel *model, const char *attrname,
                     int len, int *ind, char *newvalues);
foreign import ccall int GRBgetdblattr(GRBmodel *model, const char *attrname, double *valueP);
foreign import ccall int GRBsetdblattr(GRBmodel *model, const char *attrname, double newvalue);
foreign import ccall int GRBgetdblattrelement(GRBmodel *model, const char *attrname,
                       int element, double *valueP);
foreign import ccall int GRBsetdblattrelement(GRBmodel *model, const char *attrname,
                       int element, double newvalue);
foreign import ccall int GRBgetdblattrarray(GRBmodel *model, const char *attrname,
                     int first, int len, double *values);
foreign import ccall int GRBsetdblattrarray(GRBmodel *model, const char *attrname,
                     int first, int len, double *newvalues);
foreign import ccall int GRBgetdblattrlist(GRBmodel *model, const char *attrname,
                    int len, int *ind, double *values);
foreign import ccall int GRBsetdblattrlist(GRBmodel *model, const char *attrname,
                    int len, int *ind, double *newvalues);
foreign import ccall int GRBgetstrattr(GRBmodel *model, const char *attrname, char **valueP);
foreign import ccall int GRBsetstrattr(GRBmodel *model, const char *attrname, const char *newvalue);
foreign import ccall int GRBgetstrattrelement(GRBmodel *model, const char *attrname,
                       int element, char **valueP);
foreign import ccall int GRBsetstrattrelement(GRBmodel *model, const char *attrname,
                       int element, const char *newvalue);
foreign import ccall int GRBgetstrattrarray(GRBmodel *model, const char *attrname,
                     int first, int len, char **values);
foreign import ccall int GRBsetstrattrarray(GRBmodel *model, const char *attrname,
                     int first, int len, char **newvalues);
foreign import ccall int GRBgetstrattrlist(GRBmodel *model, const char *attrname,
                    int len, int *ind, char **values);
foreign import ccall int GRBsetstrattrlist(GRBmodel *model, const char *attrname,
                    int len, int *ind, char **newvalues);
foreign import ccall int GRBsetcallbackfunc(GRBmodel *model,
                     int (__stdcall *cb)(CB_ARGS),
                     void  *usrdata);
foreign import ccall int GRBgetcallbackfunc(GRBmodel *model,
                     int (__stdcall **cbP)(CB_ARGS));
foreign import ccall int GRBsetlogcallbackfunc(GRBmodel *model,
                        int (__stdcall *cb)(char *msg));
foreign import ccall int GRBsetlogcallbackfuncenv(GRBenv *env,
                           int (__stdcall *cb)(char *msg));
foreign import ccall int GRBcbget(void *cbdata, int where, int what, void *resultP);
foreign import ccall int GRBcbsetparam(void *cbdata, const char *paramname, const char *newvalue);
foreign import ccall int GRBcbsolution(void *cbdata, const double *solution);
foreign import ccall int GRBcbcut(void *cbdata, int cutlen, const int *cutind, const double *cutval,
          char cutsense, double cutrhs);
foreign import ccall int GRBcblazy(void *cbdata, int lazylen, const int *lazyind,
            const double *lazyval, char lazysense, double lazyrhs);
-}

-- Sorry!
