--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.Syn (
  Literal(..),
  ArgExpr(..),
  LetExpr(..),
  AltCon(..),
  FunBody(..),
  LocalVarBind(..),
  FunBodyResult(..),
  Bind(..),
  DataConInst(..),
  DataFieldInst(..),
  FunInst(..)
) where

import Data.Int
import FastString
import UniqSet
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
  | LocalVarFun GlobalVarId [ValueType] LocalVarId
  | IntBox ArgExpr
  | FloatBox ArgExpr
  | FunApp ArgExpr ArgExpr
  | InstFunApp GlobalVarId [ValueType] [ArgExpr]
  | LetFunApp FunBody
  | LamFun [LocalVarId] FunBody
  | CaseFunApp ArgExpr [(AltCon, (LocalVarId, [LocalVarId], FunBody))]
  | ArgArray [ArgExpr]
  | ArgExpr ArgExpr
  | LetIf ArgExpr LetExpr LetExpr

data AltCon =
    DataAltCon Int32
  | EnumAltCon Int32
  | BoxAltCon
  | LitAltCon Literal
  | DefaultAltCon

data FunBody =
    Let [LocalVarBind] FunBodyResult

data LocalVarBind =
    LvarBind LocalVarId LetExpr
  | ClosureVarBind LocalVarId ArgExpr

data FunBodyResult =
    Ret LetExpr
  | Retry [ArgExpr]
  | FunBodyResultIf ArgExpr FunBodyResult FunBodyResult

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
      b_closureVarIds :: UniqSet LocalVarId,
      b_funInsts :: [FunInst]
    }

data DataConInst =
    DataConInst {
      dci_typeParamTypes :: [ValueType]
    }

data DataFieldInst =
    DataFieldInst {
      dfi_typeParamType :: ValueType
    }

data FunInst =
    FunInst {
      fi_typeParamTypes :: [ValueType],
      fi_argIds :: [LocalVarId],
      fi_body :: FunBody,
      fi_closureVarIds :: UniqSet LocalVarId
    }
