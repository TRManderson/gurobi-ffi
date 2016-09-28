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