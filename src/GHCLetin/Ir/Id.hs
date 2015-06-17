--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.Id (
  LocalVarId(..),
  GlobalVarId(..)
) where

import qualified Var as GHC

data LocalVarId =
    LocalVarId Int (Maybe GHC.Id)

data GlobalVarId =
    GlobalVarId GHC.Id
