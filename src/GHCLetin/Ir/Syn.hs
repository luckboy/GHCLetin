--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.Syn (
  Literal(..),
  ArgExpr(..),
  LetExpr(..),
  FunBody(..),
  LocalVarBind(..),
  FunBodyResult(..),
  Bind(..),
  DataConInst(..),
  FunInst(..)
) where

import Data.Int
import FastString
import GHCLetin.Ir.Id
import GHCLetin.Ir.Type
import GHCLetin.Letin.Type

data Literal =
    Int Int64
  | Float Double
  | String FastString

data ArgExpr =
    Lit Literal
  | Arg Int LocalVarId
  | Lvar Int LocalVarId
  | Gvar GlobalVarId
  | Unbox ArgExpr
  | LetExpr NodeId LetExpr
  | ArgIf ArgExpr ArgExpr ArgExpr

data LetExpr =
    LetLvar Int LocalVarId
  | IntBox ArgExpr
  | FloatBox ArgExpr
  | FunApp ArgExpr ArgExpr
  | InstFunApp GlobalVarId [ValueType] [ArgExpr]
  | LetFunApp FunBody
  | LamFun [LocalVarId] FunBody
  | CaseFunApp [(Int32, (LocalVarId, [LocalVarId], FunBody))] (Maybe (LocalVarId, [LocalVarId], FunBody))
  | ArgArray [ArgExpr]
  | ArgExpr ArgExpr
  | LetIf ArgExpr LetExpr LetExpr

data FunBody =
    Let [LocalVarBind] FunBodyResult

data LocalVarBind =
    LvarBind LocalVarId LetExpr
  | ClosureVarBind LocalVarId ArgExpr

data FunBodyResult =
    Ret LetExpr
  | Retry [ArgExpr]
  | TailRecCaseRet LetExpr
  | TailRecCaseRetry NodeId LetExpr

data Bind =
    DataConBind {
      b_id :: GlobalVarId,
      b_dataConInsts :: [DataConInst]
    }
  | DataFieldBind {
      b_id :: GlobalVarId,
      b_dataFieldInsts :: [DataFieldInst]
    }
  | FunBind {
      b_id :: GlobalVarId,
      b_argIds :: [LocalVarId],
      b_body :: FunBody,
      b_funInsts :: [FunInst]
    }

data DataConInst =
    DataConInst {
      dci_typeParamTypes :: [ValueType]
    }

data DataFieldInst =
    DataFieldInst {
      dfi_typeParamIndex :: Int,
      dfi_typeParamType :: ValueType
    }

data FunInst =
    FunInst {
      fi_typeParamTypes :: [ValueType],
      fi_body :: FunBody
    }
