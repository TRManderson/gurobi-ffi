{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Gurobi.Internal where

import Foreign
import Foreign.C.Types

x = 2

#include <gurobi_c.h>

