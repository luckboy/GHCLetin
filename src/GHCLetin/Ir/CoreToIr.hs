--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.CoreToIr (
  coreToIr
) where

import Control.Monad
import Data.Array
import Data.Char
import Data.Int
import Data.Maybe
import UniqFM
import UniqSet
import qualified DynFlags as GHC
import qualified CoreSyn as GHC
import qualified Literal as GHC
import qualified Name as GHC
import qualified TyCon as GHC
import qualified Type as GHC
import qualified Var as GHC
import qualified Var
import GHCLetin.Ir.Id
import GHCLetin.Ir.Syn
import GHCLetin.Ir.Type
import GHCLetin.Letin.Type

coreToIr :: UniqSet FunName -> [GHC.TyCon] -> GHC.CoreProgram -> IO [Bind]
coreToIr funNames tyCons prog = undefined

tyConToIrBinds :: GHC.TyCon -> [Bind]
tyConToIrBinds tyCons = undefined

coreBindToIrBind :: UniqSet FunName -> GHC.CoreBind -> Bind
coreBindToIrBind funNames bind = undefined

coreExprToIrFunBody :: UniqSet FunName -> Array Int ValueType -> GHC.CoreExpr -> FunBody
coreExprToIrFunBody funNames tyPaTypes expr = undefined

data IrEnv =
    IrEnv {
      ie_funNames :: UniqSet FunName,
      ie_typeParams :: UniqFM Int,
      ie_typeParamTypes :: Array Int ValueType,
      ie_localVarInfos :: UniqFM IrLocalVarInfo,
      ie_argIds :: UniqSet LocalVarId,
      ie_lvarIds :: UniqSet LocalVarId,
      ie_closureIndex :: Int,
      ie_funPairMaybe :: Maybe (GHC.Var, [ValueType]),
      ie_case :: Bool
    }

data IrLocalVarInfo =
    IrLocalVarInfo {
      ilvi_localVarId :: LocalVarId,
      ilvi_closureIndex :: Int
    }

initIrEnv :: UniqSet FunName -> UniqFM Int -> Array Int ValueType -> Maybe (GHC.Var, [ValueType]) -> IrEnv
initIrEnv funNames tyParams tyPaTypes funPairMaybe =
  IrEnv {
    ie_funNames = funNames,
    ie_typeParams = tyParams,
    ie_typeParamTypes = tyPaTypes,
    ie_localVarInfos = emptyUFM,
    ie_argIds = emptyUniqSet,
    ie_lvarIds = emptyUniqSet,
    ie_closureIndex = 0,
    ie_funPairMaybe = funPairMaybe,
    ie_case = False
  }

addLocalVarInfos :: IrEnv -> [(GHC.Var, IrLocalVarInfo)] -> IrEnv
addLocalVarInfos env infos = env { ie_localVarInfos = addListToUFM (ie_localVarInfos env) infos }

setArgIds :: IrEnv -> [LocalVarId] -> IrEnv
setArgIds env ids = env { ie_argIds =  mkUniqSet ids }

setLvarIds :: IrEnv -> [LocalVarId] -> IrEnv
setLvarIds env ids = env { ie_lvarIds =  mkUniqSet ids }

incClosureIndex :: IrEnv -> IrEnv
incClosureIndex env = env { ie_closureIndex = (ie_closureIndex env) + 1 }

setFunPairMaybe :: IrEnv ->  Maybe (GHC.Var, [ValueType]) -> IrEnv
setFunPairMaybe env pairMaybe = env { ie_funPairMaybe = pairMaybe }

coreExprToIrFunBody' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (FunBody, ValueType, (Int, UniqSet LocalVarId))
coreExprToIrFunBody' env expr (i, closureVarIds) =
  let (binds, retExpr) = splitCoreLets expr
      (localVarInfos, i') = coreBindsToIrLocalVarInfos env binds i
      lvarIds = map (ilvi_localVarId . snd) localVarInfos
  in  undefined

coreExprToIrFunBodyResult' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (FunBodyResult, ValueType, (Int, UniqSet LocalVarId))
coreExprToIrFunBodyResult' env expr pair = undefined

coreExprToIrArgExpr' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
coreExprToIrArgExpr' env expr pair =
  let env' = setFunPairMaybe env Nothing
  in  case expr of
        GHC.Var v    -> varToIrArgExpr' env' v pair
        GHC.Lit l    -> literalToIrArgExpr' l pair
        GHC.Cast e _ -> coreExprToIrArgExpr' env' e pair
        GHC.Tick _ e -> coreExprToIrArgExpr' env' e pair
        _         ->
          case coreCaseToIrArgIf'_maybe env' expr pair of
            Just t  -> t
            Nothing ->
              case coreExprToIrLetExpr' env expr pair of
                (e, vt, (i', cvids)) -> (LetExpr (NodeId i' vt) e, vt, (i' + 1, cvids))

varToIrArgExpr' :: IrEnv -> GHC.Var -> (Int, UniqSet LocalVarId) -> (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
varToIrArgExpr' env var (i, closureVarIds) =
  let (expr, valueType, closureVarIds') =
        case lookupUFM (ie_localVarInfos env) var of
          Just info ->
            let id = ilvi_localVarId info
            in  if isJust (lookupUniqSet (ie_argIds env) id) then
                  (Arg (ilvi_closureIndex info) id, valueType, closureVarIds)
                else if isJust (lookupUniqSet (ie_lvarIds env) id) then
                  (Lvar (ilvi_closureIndex info) id, valueType, closureVarIds)
                else
                  (Lvar (ilvi_closureIndex info) id, valueType, addOneToUniqSet closureVarIds id)
          Nothing   ->
            (Gvar (varToGlobalVarId (ie_typeParams env) var), ValueTypeRef, closureVarIds)
  in  (expr, valueType, (i, closureVarIds'))

literalToIrArgExpr' :: GHC.Literal -> (Int, UniqSet LocalVarId) -> (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
literalToIrArgExpr' lit pair =
  let (expr, valueType) =
        case lit of
          GHC.MachChar x      -> (Lit (Int (fromInteger (toInteger (ord x)))), ValueTypeInt)
          GHC.MachStr x       -> (Lit (String x), ValueTypeRef)
          GHC.MachNullAddr    -> (Lit (Int 0), ValueTypeInt)
          GHC.MachInt x       -> (Lit (Int (fromInteger x)), ValueTypeInt)
          GHC.MachInt64 x     -> (Lit (Int (fromInteger x)), ValueTypeInt)
          GHC.MachWord x      -> (Lit (Int (fromInteger x)), ValueTypeInt)
          GHC.MachWord64 x    -> (Lit (Int (fromInteger x)), ValueTypeInt)
          GHC.MachFloat x     -> (Lit (Float (fromRational x)), ValueTypeFloat)
          GHC.MachDouble x    -> (Lit (Float (fromRational x)), ValueTypeFloat)
          GHC.MachLabel _ _ _ -> (Lit (Int 0), ValueTypeInt)
          GHC.LitInteger x _  -> (Lit (Int (fromInteger x)), ValueTypeInt)
  in  (expr, valueType, pair)

coreCaseToIrArgIf'_maybe :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> Maybe (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
coreCaseToIrArgIf'_maybe env expr pair = undefined

coreExprToIrLetExpr' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreExprToIrLetExpr' env expr pair =
  let env' = setFunPairMaybe env Nothing
  in  case expr of
        GHC.Var v    -> varToIrLetExpr' env' v pair
        GHC.Lit l    -> literalToIrLetExpr' l pair
        GHC.App _ _  -> coreAppToIrLetExpr' env' expr pair
        GHC.Cast e _ -> coreExprToIrLetExpr' env' e pair
        GHC.Tick _ e -> coreExprToIrLetExpr' env' e pair
        _            ->
          case coreCaseToIrLetIf'_maybe env' expr pair of
            Just t  -> t
            Nothing ->
              case expr of
                GHC.Lam _ _ -> coreLamToIrExpr env expr pair
                _           -> coreLetToIrExpr env' expr pair

varToIrLetExpr' :: IrEnv -> GHC.Var -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
varToIrLetExpr' env var pair =
  case varToIrArgExpr' env var pair of
    (e @ (Lvar ci id), vt, p) ->
      case lookupUFM (ie_localVarInfos env) var of
        Just info ->
          if ie_closureIndex env == 0 || (ie_closureIndex env == 1 && ilvi_closureIndex info == 0) then
            (ArgExpr e, vt, p)
          else
            (LetLvar ci id, vt, p)
        Nothing  -> (ArgExpr e, vt, p)
    (e, vt, p)                -> (ArgExpr e, vt, p)

literalToIrLetExpr' :: GHC.Literal -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
literalToIrLetExpr' lit p =
  case literalToIrArgExpr' lit p of
    (e, vt, p) -> (ArgExpr e, vt, p)

coreAppToIrLetExpr' :: IrEnv -> GHC.CoreExpr-> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreAppToIrLetExpr' env expr pair =
  let (fun, args, tyVars) = splitCoreApps expr
      pairMaybe =
        case coreExprVar_maybe fun of
          Just v   ->
            let ats = (map (tyVarToIrArgType (ie_typeParams env)) tyVars)
                vts = map (instArgType (ie_typeParamTypes env)) ats
            in  if isJust (lookupUFM (ie_localVarInfos env) v) && isInstFunVar (ie_funNames env) v vts then
                  Just (v, vts)
                else
                  Nothing
          Nothing -> Nothing
  in  case pairMaybe of
        Just (v, vts) ->
          let tyParams = ie_typeParams env
              funTyPaTypes = listArray (0, length vts - 1) vts
              funType = instFunType funTyPaTypes (varIrFunType tyParams v)
              argTypes = take (length args) (ift_argTypes funType)
              retType =
                if length args == length (ift_argTypes funType) then
                  ift_retType funType
                else
                  ValueTypeRef
              funId = varToGlobalVarId (ie_typeParams env) v
              (args', pair') = foldr (
                  \a (as, p) ->
                    case coreExprToIrArgExpr' env a p of
                      (a', vt, p') ->
                        let a'' =
                              case vt of
                                ValueTypeRef -> a'
                                _            -> Unbox a'
                        in  (a'' : as, p')
                ) ([], pair) args
          in  (InstFunApp funId (elems funTyPaTypes) args', retType, pair')
        Nothing       ->
          let (fun' : args', pair') = foldr (
                  \a (as, p) ->
                    case coreExprToIrArgExpr' env a p of
                      (a', vt, (j', cvids')) ->
                        let pf e = (LetExpr (NodeId j' ValueTypeRef) e, j' + 1)
                            (a'', j'') =
                              case vt of
                                ValueTypeInt   -> pf (IntBox a')
                                ValueTypeFloat -> pf (FloatBox a')
                                ValueTypeRef   -> (a', j')
                        in  (a'' : as, (j'', cvids'))
                ) ([], pair) (fun : args)
              argArray' = LetExpr (NodeId (fst pair') ValueTypeRef) (ArgArray args')
          in  (FunApp fun' argArray', ValueTypeRef, pair')

coreCaseToIrLetIf'_maybe :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> Maybe (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreCaseToIrLetIf'_maybe env expr pair = undefined

coreLetToIrExpr :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreLetToIrExpr env expr pair =
  let env' = setFunPairMaybe (setArgIds (incClosureIndex env) []) Nothing
      (funBody, valueType, pair') = coreExprToIrFunBody' env' expr pair
  in  (LetFunApp funBody, valueType, pair')

coreLamToIrExpr :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreLamToIrExpr env expr (i, closureVarIds) =
  let (args, tyVars, bodyExpr) = splitCoreLams expr
      (localVarInfos, i') = varsToIrLocalVarInfos env args i
      args' = map (ilvi_localVarId . snd) localVarInfos
      env' = setArgIds (incClosureIndex env) args'
      (funBody, _, pair') = coreExprToIrFunBody' env' expr (i', closureVarIds)
  in  (LamFun args' funBody, ValueTypeRef, pair')

coreBindsToIrLocalVarInfos :: IrEnv -> [GHC.CoreBind] -> Int -> ([(GHC.Var, IrLocalVarInfo)], Int)
coreBindsToIrLocalVarInfos env binds i = foldr (addCoreBindIrLocalVarInfos env) ([], i) binds

addCoreBindIrLocalVarInfos :: IrEnv -> GHC.CoreBind -> ([(GHC.Var, IrLocalVarInfo)], Int) -> ([(GHC.Var, IrLocalVarInfo)], Int)
addCoreBindIrLocalVarInfos env bind (infos, i) =
  case bind of
    GHC.NonRec v e ->
      let tyParams = ie_typeParams env
          tyPaTypes = ie_typeParamTypes env
          id = LocalVarId (NodeId i (varLetinValueType tyParams tyPaTypes v)) v
      in  ((v, IrLocalVarInfo id (ie_closureIndex env)) : infos, i + 1)
    GHC.Rec ps     ->
      let (newInfos, i') = coreBindsToIrLocalVarInfos env (map (\(v, lvi) -> GHC.NonRec v lvi) ps) i
      in  (newInfos ++ infos, i')

varsToIrLocalVarInfos :: IrEnv -> [GHC.Var] -> Int -> ([(GHC.Var, IrLocalVarInfo)], Int)
varsToIrLocalVarInfos env vars i = foldr (addVarIrLocalVarInfo env) ([], i) vars

addVarIrLocalVarInfo :: IrEnv -> GHC.Var -> ([(GHC.Var, IrLocalVarInfo)], Int) -> ([(GHC.Var, IrLocalVarInfo)], Int)
addVarIrLocalVarInfo env var (infos, i) =
  let tyParams = ie_typeParams env
      tyPaTypes = ie_typeParamTypes env
      id = LocalVarId (NodeId i (varLetinValueType tyParams tyPaTypes var)) var
  in  ((var, IrLocalVarInfo id (ie_closureIndex env)) : infos, i + 1)

typeToIrFunType :: UniqFM Int -> GHC.Type -> FunType
typeToIrFunType tyParams typ = undefined

tyVarToIrArgType :: UniqFM Int -> GHC.TyVar -> ArgType
tyVarToIrArgType tyParams typ = undefined

varIrFunType :: UniqFM Int -> GHC.Var -> FunType
varIrFunType tyParams var = undefined

varIrArgType :: UniqFM Int -> GHC.Var -> ArgType
varIrArgType tyParams var = undefined

varIrRetType :: UniqFM Int -> GHC.Var -> RetType
varIrRetType = varIrArgType

varLetinValueType :: UniqFM Int -> Array Int ValueType -> GHC.Var -> ValueType
varLetinValueType tyParams tyPaTypes var = instArgType tyPaTypes (varIrArgType tyParams var)

isInstFunVar :: UniqSet FunName -> GHC.Var -> [ValueType] -> Bool
isInstFunVar funNames var valueTypes = undefined

varToGlobalVarId :: UniqFM Int -> GHC.Var -> GlobalVarId
varToGlobalVarId tyParams var =
  let fs = GHC.occNameFS (GHC.nameOccName (Var.varName var))
  in  GlobalVarId fs (varIrFunType tyParams var) var

splitCoreLams :: GHC.CoreExpr -> ([GHC.Var], [GHC.TyVar], GHC.CoreExpr)
splitCoreLams = splitCoreLams' ([], [])

splitCoreLams' :: ([GHC.Var], [GHC.TyVar]) -> GHC.CoreExpr -> ([GHC.Var], [GHC.TyVar], GHC.CoreExpr)
splitCoreLams' (pair @ (vars, tyVars)) expr =
  case expr of
    GHC.Lam a e  ->
      let pair' = if not (GHC.isTyVar a) then (a : vars, tyVars) else (vars, a : tyVars)
      in  splitCoreLams' pair' e
    GHC.Cast e _ -> splitCoreLams' pair e
    GHC.Tick _ e -> splitCoreLams' pair e
    _            -> (reverse vars, reverse tyVars, expr)

splitCoreLets :: GHC.Expr GHC.CoreBndr -> ([GHC.CoreBind], GHC.CoreExpr)
splitCoreLets = splitCoreLets' []

splitCoreLets' :: [GHC.CoreBind] -> GHC.CoreExpr -> ([GHC.CoreBind], GHC.CoreExpr)
splitCoreLets' binds expr =
  case expr of
    GHC.Let b e  -> splitCoreLets' (b : binds) e
    GHC.Cast e _ -> splitCoreLets' binds e
    GHC.Tick _ e -> splitCoreLets' binds e
    _            -> (reverse binds, expr)

splitCoreApps :: GHC.CoreExpr -> (GHC.CoreExpr, [GHC.CoreExpr], [GHC.Var])
splitCoreApps = splitCoreApps' ([], [])

splitCoreApps' :: ([GHC.CoreExpr], [GHC.Var]) -> GHC.CoreExpr -> (GHC.CoreExpr, [GHC.CoreExpr], [GHC.Var])
splitCoreApps' (pair @ (args, tyVars)) expr =
  case expr of
    GHC.App f a  ->
      let pair' =
            case coreAppArgTyVar_maybe a of
              Just tv -> (args, tv : tyVars)
              Nothing -> (a : args, tyVars)
      in  splitCoreApps' pair' f
    GHC.Cast e _ -> splitCoreApps' pair e
    GHC.Tick _ e -> splitCoreApps' pair e
    _            -> (expr, args, tyVars)

coreAppArgTyVar_maybe :: GHC.CoreExpr -> Maybe GHC.TyVar
coreAppArgTyVar_maybe expr =
  case expr of
    GHC.Var id -> if GHC.isTyVar id then Just id else Nothing
    _          -> Nothing

coreExprVar_maybe :: GHC.CoreExpr -> Maybe GHC.Var
coreExprVar_maybe expr =
  case expr of
    GHC.Var v    -> Just v
    GHC.Cast e _ -> coreExprVar_maybe e
    GHC.Tick _ e -> coreExprVar_maybe e
    _            -> Nothing
