--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.Syn (
  Literal(..),
  ArgExpr(..),
  LetExpr(..),
  FunBody(..),
  Bind(..),
  DataConInst(..),
  FunInst(..)
) where

import Data.Int
import GHCLetin.Ir.Id
import GHCLetin.Ir.Type
import GHCLetin.Letin.Type

data Literal =
    Int Int64
  | Float Double

data ArgExpr =
    Lit Literal
  | Arg LocalVarId
  | Lvar LocalVarId
  | Gvar GlobalVarId
  | Fun GlobalVarId [ValueType]
  | FunClosureVar LocalVarId
  | Unbox ArgExpr
  | LetExpr LocalVarId LetExpr
  | ArgIf ArgExpr ArgExpr ArgExpr

data LetExpr =
    Box ArgExpr
  | LamClosureVar Int LocalVarId
  | FunApp ArgExpr ArgExpr
  | InstFunApp ArgExpr [ArgExpr]
  | FunExpr FunExpr
  | ArgTuple [ArgExpr]
  | ArgExpr ArgExpr
  | LetIf ArgExpr LetExpr LetExpr

data FunBody =
    Let [(LocalVarId, LetExpr)] FunBody
  | ClosureLet [(LocalVarId, ArgExpr)] FunBody
  | Ret LetExpr
  | Retry [ArgExpr]
  | CaseRet LetExpr
  | CaseRetry LocalVarId LetExpr

data FunExpr =
    LetFun FunBody
  | LamFun [LocalVarId] FunBody
  | CaseFun [(Int32, (LocalVarId, [LocalVarId], FunBody))] (Maybe (LocalVarId, [LocalVarId], FunBody))

data Bind =
    DataConBind {
      b_id :: GlobalVarId,
      b_funType :: FunType,
      b_dataConInsts :: [DataConInst]
    }
  | FunBind {
      b_id :: GlobalVarId,
      b_argIds :: [LocalVarId],
      b_funType :: FunType,
      b_body :: FunBody,
      b_funInsts :: [FunInst]
    }

data DataConInst =
    DataConInst {
      dci_paramTypes :: [ValueType]
    }

data FunInst =
    FunInst {
      fi_paramTypes :: [ValueType],
      fi_body :: FunBody
    }
