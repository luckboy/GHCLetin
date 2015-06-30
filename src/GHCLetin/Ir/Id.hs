--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.Id (
  NodeId(..),
  LocalVarId(..),
  GlobalVarId(..),
  FunName(..),
  mkFunName,
  mkInstFunName
) where

import Data.Array
import FastString
import Unique
import qualified Var as GHC
import GHCLetin.Ir.Type
import GHCLetin.Letin.Type

data NodeId =
    NodeId {
      ni_i :: Int,
      ni_valueType :: ValueType
    }

instance Uniquable NodeId where
  getUnique = getUnique . ni_i

data LocalVarId =
    LocalVarId {
      lvi_nodeId :: NodeId,
      lvi_var :: GHC.Var
    }

instance Uniquable LocalVarId where
  getUnique = getUnique . lvi_nodeId

data GlobalVarId =
    GlobalVarId {
      gvi_fs :: FastString,
      gvi_funType :: FunType,
      gvi_var :: GHC.Var
    }

instance Uniquable GlobalVarId where
  getUnique = getUnique . gvi_fs

data FunName =
    FunName {
      fn_fs :: FastString
    }

instance Uniquable FunName where
  getUnique = getUnique . fn_fs

valueTypeToChar :: ValueType -> Char
valueTypeToChar ValueTypeInt   = 'i'
valueTypeToChar ValueTypeFloat = 'f'
valueTypeToChar ValueTypeRef   = 'r'

mkFunName :: GlobalVarId -> Int -> FunName
mkFunName id argCount = FunName (mkFastString ("$f" ++ (show argCount) ++ "$" ++ (unpackFS (gvi_fs id))))

mkInstFunName :: GlobalVarId -> Array Int ValueType -> Int -> FunName
mkInstFunName id tyPaTypes argCount =
  let len  = snd (bounds tyPaTypes) - fst (bounds tyPaTypes)
      cs = map valueTypeToChar (elems tyPaTypes)
      s = unpackFS (gvi_fs id)
  in  FunName (mkFastString ("$if" ++ (show len) ++ cs ++ (show argCount) ++ "$" ++ s))
