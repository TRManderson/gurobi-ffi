module Numeric.Gurobi where
import Foreign
import Foreign.C
import Numeric.Gurobi.Types
import qualified Numeric.Gurobi.C as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except


withFinalPtr :: Storable a => a -> (Ptr a -> IO b) -> IO b
withFinalPtr def transformation = do
  ptr <- new def
  fp <- newForeignPtr finalizerFree ptr
  withForeignPtr fp transformation


loadEnv :: String -> Gurobi Env
loadEnv logFileName = do
  (cEnv, (CInt status)) <- lift . withCString logFileName $ \fname -> do
    withFinalPtr nullPtr $ \h -> do
      status <- C.loadEnv h fname
      peekResult <- maybePeek (peek) h
      return (peekResult, status)

  case status of
    0 -> case cEnv of
      Just env -> return . Env $ env
      Nothing -> throwE NotImplementedError
    _ -> throwE NotImplementedError

loadClientEnv :: String -> [String] -> Int -> String -> Int -> Double -> IO Env
loadClientEnv logFile computeServers port password priority timeout = undefined

freeEnv :: Env -> IO ()
freeEnv (Env e) = C.freeEnv e

getConcurrentEnv :: Model -> Int -> IO (Maybe Env)
getConcurrentEnv (Model m) num = do
  result <- C.getConcurrentEnv m (fromIntegral num)
  return $ case (result) of
    nullPtr -> Nothing
    _       -> Just (Env result)

discardConcurrentEnvs :: Model -> IO ()
discardConcurrentEnvs (Model m) = C.discardConcurrentEnvs m

loadModel :: Env -> String -> Int -> Int -> ObjectiveSense -> Double -> [Double] -> [ConstraintSense] -> [Double] -> [Int] -> [Int] -> [Int] -> [Double] -> [Double] -> [Double] -> [VariableType] -> [String] -> [String] -> Gurobi Model
loadModel (Env env) name numVars numConstrs objSense objOffset objCoeffs constrSenses constrRHS vBeg vLen vVal lbs ubs vtypes varnames constrnames = undefined

newModel :: Env -> String -> [Variable] -> Gurobi Model
newModel (Env e) name vars = do
  return undefined
