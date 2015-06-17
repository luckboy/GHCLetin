--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.Type (
  ArgType(..),
  RetType,
  FunType(..)
) where

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
