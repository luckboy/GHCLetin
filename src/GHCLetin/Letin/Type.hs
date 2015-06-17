--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Letin.Type (
  ValueType(..),
  ObjectType(..)
) where

data ValueType =
    ValueTypeInt
  | ValueTypeFloat
  | ValueTypeRef

data ObjectType =
    ObjectTypeIarray8
  | ObjectTypeIarray16
  | ObjectTypeIarray32
  | ObjectTypeIarray64
  | ObjectTypeSfarray
  | ObjectTypeDfarray
  | ObjectTypeRarray
  | ObjectTypeTuple
