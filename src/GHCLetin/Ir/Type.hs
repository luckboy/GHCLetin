--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.Type (
  ArgType(..),
  RetType,
  FunType(..),
  InstFunType(..),
  instFunType,
  instArgType,
  instRetType
) where

import Data.Array
import GHCLetin.Letin.Type

data ArgType =
    ValueType ValueType (Maybe ValueType)
  | TypeParam Int

type RetType = ArgType

data FunType =
    FunType {
      ft_typeParamCount :: Int,
      ft_argTypes :: [ArgType],
      ft_retType :: RetType
    }

data InstFunType =
    InstFunType {
      ift_argTypes :: [ValueType],
      ift_retType :: ValueType
    }

instFunType :: Array Int ValueType -> FunType -> InstFunType
instFunType tyPaTypes funType =
  InstFunType {
    ift_argTypes = map (instArgType tyPaTypes) (ft_argTypes funType),
    ift_retType = instRetType tyPaTypes (ft_retType funType)
  }

instArgType :: Array Int ValueType -> ArgType -> ValueType
instArgType tyPaTypes argType =
  case argType of
    ValueType vt _ -> vt
    TypeParam i    ->
      let (j, k) = bounds tyPaTypes
      in  if i >= j && i <= k then tyPaTypes ! i else ValueTypeRef

instRetType :: Array Int ValueType -> ArgType -> ValueType
instRetType = instArgType
