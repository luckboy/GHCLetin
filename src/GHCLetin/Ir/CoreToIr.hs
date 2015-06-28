--
-- Copyright: (c) 2015 Łukasz Szpakowski
-- License: MIT
--

module GHCLetin.Ir.CoreToIr (
  coreToIr
) where

import Data.Array
import Data.Char
import Data.Int
import Data.Maybe
import UniqFM
import UniqSet
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified CoreSyn as GHC
import qualified Literal as GHC
import qualified Name as GHC
import qualified TyCon as GHC
import qualified Type as GHC
import qualified TysPrim as GHC
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
      ie_origFunId :: GlobalVarId,
      ie_typeParams :: UniqFM Int,
      ie_typeParamTypes :: Array Int ValueType,
      ie_localVarInfos :: UniqFM IrLocalVarInfo,
      ie_argIds :: UniqSet LocalVarId,
      ie_lvarIds :: UniqSet LocalVarId,
      ie_closureIndex :: Int,
      ie_funPairMaybe :: Maybe (GHC.Var, Int),
      ie_funFlag :: Bool
    }

data IrLocalVarInfo =
    IrLocalVarInfo {
      ilvi_localVarId :: LocalVarId,
      ilvi_closureIndex :: Int,
      ilvi_recursive :: Bool
    }

initIrEnv :: UniqSet FunName -> GlobalVarId -> UniqFM Int -> Array Int ValueType -> Maybe (GHC.Var, Int) -> IrEnv
initIrEnv funNames origFunId tyParams tyPaTypes funPairMaybe =
  IrEnv {
    ie_funNames = funNames,
    ie_origFunId = origFunId,
    ie_typeParams = tyParams,
    ie_typeParamTypes = tyPaTypes,
    ie_localVarInfos = emptyUFM,
    ie_argIds = emptyUniqSet,
    ie_lvarIds = emptyUniqSet,
    ie_closureIndex = 0,
    ie_funPairMaybe = funPairMaybe,
    ie_funFlag = False
  }

addLocalVarInfos :: IrEnv -> [(GHC.Var, IrLocalVarInfo)] -> IrEnv
addLocalVarInfos env infos = env { ie_localVarInfos = addListToUFM (ie_localVarInfos env) infos }

setArgIds :: IrEnv -> [LocalVarId] -> IrEnv
setArgIds env ids = env { ie_argIds =  mkUniqSet ids }

setLvarIds :: IrEnv -> [LocalVarId] -> IrEnv
setLvarIds env ids = env { ie_lvarIds =  mkUniqSet ids }

incClosureIndex :: IrEnv -> IrEnv
incClosureIndex env = env { ie_closureIndex = (ie_closureIndex env) + 1 }

setFunPairMaybe :: IrEnv ->  Maybe (GHC.Var, Int) -> IrEnv
setFunPairMaybe env pairMaybe = env { ie_funPairMaybe = pairMaybe }

setFunFlag :: IrEnv -> Bool -> IrEnv
setFunFlag env funFlag = env { ie_funFlag = funFlag }

coreExprToIrFunBody' :: IrEnv -> GHC.CoreExpr -> Maybe ValueType -> (Int, UniqSet LocalVarId) -> (FunBody, ValueType, (Int, UniqSet LocalVarId))
coreExprToIrFunBody' env expr valueTypeMaybe (i, closureVarIds) =
  let (binds, retExpr) = splitCoreLets expr
      (localVarInfos, i') = coreBindsToIrLocalVarInfos env binds i
      lvarIds = map (ilvi_localVarId . snd) localVarInfos
      env' = setLvarIds (addLocalVarInfos env localVarInfos) lvarIds
      (funBodyRes', valueType, pair') = coreExprToIrFunBodyResult' env' retExpr valueTypeMaybe (i', emptyUniqSet)
      (i'', closureVarIds') = pair'
      bindExprs = coreBindsToCoreExprs binds
      bindTuples = map (\((v, lvi), (_, e)) -> (v, lvi, e)) (zip localVarInfos bindExprs)
      (binds', (i''', closureVarIds2)) = foldr (
          \(v, lvi, e) (bs, p) ->
            let id = ilvi_localVarId lvi
                vt = ni_valueType (lvi_nodeId id)
                (_, as, _) = splitCoreLams e
                env'' = setFunPairMaybe env' (Just (v, length as))
                (b, p''') =
                  if isJust (lookupUniqSet closureVarIds' id) then
                    let (e', vt2, (j', cvids')) =
                          if not (ilvi_recursive lvi) then
                            coreExprToIrLetExpr' env'' e p
                          else
                            coreLamToIrLetExpr' env'' e p
                        (e'', j'') =
                          case e' of
                            LamFun [] _ ->
                              let aa = LetExpr (NodeId (j' + 1) ValueTypeRef) (ArgArray [])
                              in  (FunApp (LetExpr (NodeId j' vt2) e') aa, j' + 2)
                            _           -> (e', j')
                        (e''', j''') = boxOrUnboxIrLetExpr e' vt vt2 j'
                    in  (LvarBind id e''', (j''', cvids'))
                  else
                    let (e', vt2, (j', cvids')) =
                          if not (ilvi_recursive lvi) then
                            coreExprToIrArgExpr' env'' e p
                          else
                            coreLamToIrArgExpr' env'' e p
                        (e'', j'') =
                          case e' of
                            LetExpr _ (LamFun [] _) ->
                              let aa = LetExpr (NodeId (j' + 1) ValueTypeRef) (ArgArray [])
                              in  (LetExpr (NodeId j' vt2) (FunApp e' aa), j' + 2)
                            _                       -> (e', j')
                        (e''', j''') = boxOrUnboxIrArgExpr e'' vt vt2 j''
                    in  (ClosureVarBind id e''', (j''', cvids'))
            in  (b : bs, p''')
       ) ([], pair') bindTuples
  in  (Let binds' funBodyRes', valueType, (i''', closureVarIds `unionUniqSets` closureVarIds2))

coreExprToIrFunBodyResult' :: IrEnv -> GHC.CoreExpr -> Maybe ValueType -> (Int, UniqSet LocalVarId) -> (FunBodyResult, ValueType, (Int, UniqSet LocalVarId))
coreExprToIrFunBodyResult' env expr valueTypeMaybe pair =
  case coreCaseToSimpleCase'_maybe (coreExprToIrFunBodyResult' env) FunBodyResultIf env expr pair of
    Just t  -> t
    Nothing ->
      let (fun, args, tyVars) = splitCoreApps expr
      in  case fun of
            GHC.Var v | maybe False (\(w, ac) -> v == w && length args == ac) (ie_funPairMaybe env) ->
              let (args', (i', closureVarIds')) = foldr (
                      \a (as, p) ->
                        case coreExprToIrArgExpr' (setFunFlag env False) a p of
                          (a', vt, p') ->
                            let a'' =
                                  case vt of
                                    ValueTypeRef -> a'
                                    _            -> Unbox a'
                            in  (a'' : as, p')
                    ) ([], pair) args
              in  (Retry args', maybe ValueTypeRef id valueTypeMaybe, (i' + 1, closureVarIds'))
            _         ->
              case valueTypeMaybe of
                Just vt ->
                  let (expr', valueType2, (i', closureVarIds')) = coreExprToIrLetExpr' env expr pair
                      (expr'', i'') = boxOrUnboxIrLetExpr expr' vt valueType2 i'
                  in  (Ret expr'', vt, (i'', closureVarIds'))
                Nothing ->
                  let (expr', valueType, pair') = coreExprToIrLetExpr' env expr pair
                  in  (Ret expr', valueType, pair')

coreExprToIrArgExpr' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
coreExprToIrArgExpr' env expr pair =
  let env' = setFunPairMaybe env Nothing
  in  case expr of
        GHC.Var v    -> varToIrArgExpr' env' v pair
        GHC.Lit l    -> literalToIrArgExpr' l pair
        GHC.Cast e _ -> coreExprToIrArgExpr' env' e pair
        GHC.Tick _ e -> coreExprToIrArgExpr' env' e pair
        _            ->
          case coreCaseToIrArgSimpleCase'_maybe env' expr pair of
            Just t  -> t
            Nothing ->
              case coreExprToIrLetExpr' env expr pair of
                (e, vt, (i', cvids)) -> (LetExpr (NodeId i' vt) e, vt, (i' + 1, cvids))

varToIrArgExpr' :: IrEnv -> GHC.Var -> (Int, UniqSet LocalVarId) -> (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
varToIrArgExpr' env var (pair @ (i, closureVarIds)) =
  case lookupUFM (ie_localVarInfos env) var of
    Just info ->
      if not (ilvi_recursive info) then
        let id = ilvi_localVarId info
            valueType = ni_valueType (lvi_nodeId id)
        in  if isJust (lookupUniqSet (ie_argIds env) id) then
              (Arg (ilvi_closureIndex info) id, valueType, pair)
            else if isJust (lookupUniqSet (ie_lvarIds env) id) then
              (Lvar (ilvi_closureIndex info) id, valueType, pair)
            else
              (Lvar (ilvi_closureIndex info) id, valueType, (i, addOneToUniqSet closureVarIds id))
      else
        case varToIrLocalVarFunOrFunApp' env var info pair of
          (e, vt, (i', cvids)) -> (LetExpr (NodeId i' vt) e, vt, (i' + 1, cvids))
    Nothing   ->
      (Gvar (varToGlobalVarId (ie_typeParams env) var), ValueTypeRef, pair)

literalToIrArgExpr' :: GHC.Literal -> (Int, UniqSet LocalVarId) -> (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
literalToIrArgExpr' lit pair =
  let (lit', valueType) = literalToIrLiteralWithLetinValueType lit
  in  (Lit lit', valueType, pair)

coreCaseToIrArgSimpleCase'_maybe :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> Maybe (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
coreCaseToIrArgSimpleCase'_maybe env =
  coreCaseToSimpleCase'_maybe (
      \e vtm p ->
        case vtm of
          Just vt ->
            let (e', vt2, (i, cvids)) = coreExprToIrArgExpr' env e p
                (e'', i') = boxOrUnboxIrArgExpr e' vt vt2 i
            in  (e'', vt, (i', cvids))
          Nothing -> coreExprToIrArgExpr' env e p
    ) ArgIf env

coreLamToIrArgExpr' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (ArgExpr, ValueType, (Int, UniqSet LocalVarId))
coreLamToIrArgExpr' env expr pair =
  case coreLamToIrLetExpr' env expr pair of
    (e, vt, p @ (i', cvids)) -> (LetExpr (NodeId i' vt) e, vt, (i' + 1, cvids))

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
          case coreCaseToIrLetSimpleCase'_maybe env' expr pair of
            Just t  -> t
            Nothing ->
              case expr of
                GHC.Lam _ _      -> coreLamToIrLetExpr' env expr pair
                GHC.Case _ _ _ _ -> coreCaseToIrLetExpr' env' expr pair
                _                -> coreLetToIrLetExpr' env' expr pair

varToIrLetExpr' :: IrEnv -> GHC.Var -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
varToIrLetExpr' env var pair =
  case lookupUFM (ie_localVarInfos env) var of
    Just info ->
      if not (ilvi_recursive info) then
        case varToIrArgExpr' env var pair of
          (e @ (Lvar ci id), vt, p) ->
            if ie_closureIndex env == 0 || (ie_closureIndex env == 1 && ilvi_closureIndex info == 0) then
              (ArgExpr e, vt, p)
            else
              (LetLvar ci id, vt, p)
          (e, vt, p)                -> (ArgExpr e, vt, p)
      else
        varToIrLocalVarFunOrFunApp' env var info pair
    Nothing  ->
      case varToIrArgExpr' env var pair of
        (e, vt, p) -> (ArgExpr e, vt, p)

varToIrLocalVarFunOrFunApp' :: IrEnv -> GHC.Var -> IrLocalVarInfo -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
varToIrLocalVarFunOrFunApp' env var info pair =
  if ie_funFlag env then
    varToIrLocalVarFun' env var info pair
  else
    case varToIrLocalVarFun' env var info pair of
      (e, vt, p @ (i', cvids')) ->
        (FunApp (LetExpr (NodeId (i' + 1) ValueTypeRef) e) (LetExpr (NodeId i' ValueTypeRef) (ArgArray [])), ValueTypeRef, (i' + 2, cvids'))

varToIrLocalVarFun' :: IrEnv -> GHC.Var -> IrLocalVarInfo -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
varToIrLocalVarFun' env var info pair =
  let origFunId = ie_origFunId env
      tyPaTypes = ie_typeParamTypes env
      id = ilvi_localVarId info
  in  (LocalVarFun origFunId (elems tyPaTypes) id, ValueTypeRef, pair)

literalToIrLetExpr' :: GHC.Literal -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
literalToIrLetExpr' lit p =
  case literalToIrArgExpr' lit p of
    (e, vt, p) -> (ArgExpr e, vt, p)

coreAppToIrLetExpr' :: IrEnv -> GHC.CoreExpr-> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreAppToIrLetExpr' env expr pair =
  let (fun, args, tyVars) = splitCoreApps expr
      pairMaybe =
        case coreExprVar_maybe fun of
          Just v  ->
            let tyParams = ie_typeParams env
                tyPaTypes = ie_typeParamTypes env
                valueTypes = map (tyVarLetinValueType tyParams tyPaTypes) tyVars
            in  if isJust (lookupUFM (ie_localVarInfos env) v) && isInstFunVar (ie_funNames env) v (length args) valueTypes then
                  Just (v, valueTypes)
                else
                  Nothing
          Nothing -> Nothing
  in  case pairMaybe of
        Just (v, vts) ->
          let tyParams = ie_typeParams env
              funTyPaTypes = listArray (0, length vts - 1) vts
              funType = instFunType funTyPaTypes (varIrFunType v)
              argTypes = take (length args) (ift_argTypes funType)
              retType =
                if length args == length (ift_argTypes funType) then
                  ift_retType funType
                else
                  ValueTypeRef
              funId = varToGlobalVarId (ie_typeParams env) v
              (args', pair') = foldr (
                  \(a, at) (as, p) ->
                    case coreExprToIrArgExpr' (setFunFlag env False) a p of
                      (a', vt, p') ->
                        let a'' =
                              case (at, vt) of
                                (ValueTypeRef, ValueTypeRef) -> a'
                                (_, ValueTypeRef)            -> Unbox a'
                                _                            -> a'
                        in  (a'' : as, p')
                ) ([], pair) (zip args argTypes)
          in  (InstFunApp funId (elems funTyPaTypes) args', retType, pair')
        Nothing       ->
          let (fun' : args', (i', closureVarIds')) = foldr (
                  \(a, f) (as, p) ->
                    case coreExprToIrArgExpr' (setFunFlag env f) a p of
                      (a', vt, (j', cvids')) ->
                        let pf e = (LetExpr (NodeId j' ValueTypeRef) e, j' + 1)
                            (a'', j'') =
                              case vt of
                                ValueTypeInt   -> pf (IntBox a')
                                ValueTypeFloat -> pf (FloatBox a')
                                ValueTypeRef   -> (a', j')
                        in  (a'' : as, (j'', cvids'))
                ) ([], pair) ((fun, True) : (zip args (replicate (length args) False)))
              argArray' = LetExpr (NodeId i' ValueTypeRef) (ArgArray args')
          in  (FunApp fun' argArray', ValueTypeRef, (i' + 1, closureVarIds'))

coreCaseToIrLetSimpleCase'_maybe :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> Maybe (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreCaseToIrLetSimpleCase'_maybe env =
  coreCaseToSimpleCase'_maybe (
      \e vtm p ->
        case vtm of
          Just vt ->
            let (e', vt2, (i, cvids)) = coreExprToIrLetExpr' env e p
                (e'', i') = boxOrUnboxIrLetExpr e' vt vt2 i
            in  (e'', vt, (i', cvids))
          Nothing -> coreExprToIrLetExpr' env e p
    ) LetIf env

coreLetToIrLetExpr' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreLetToIrLetExpr' env expr pair =
  let env' = setFunPairMaybe (setArgIds (incClosureIndex env) []) Nothing
      (funBody, valueType, pair') = coreExprToIrFunBody' env' expr Nothing pair
  in  (LetFunApp funBody, valueType, pair')

coreLamToIrLetExpr' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreLamToIrLetExpr' env expr (i, closureVarIds) =
  let (args, _, bodyExpr) = splitCoreLams expr
      (localVarInfos, i') = varsToIrLocalVarInfos env args i
      args' = map (ilvi_localVarId . snd) localVarInfos
      env' = setArgIds (addLocalVarInfos (incClosureIndex env) localVarInfos) args'
      (funBody, _, pair') = coreExprToIrFunBody' env' expr Nothing (i', closureVarIds)
  in  (LamFun args' funBody, ValueTypeRef, pair')

coreCaseToIrLetExpr' :: IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> (LetExpr, ValueType, (Int, UniqSet LocalVarId))
coreCaseToIrLetExpr' env expr pair =
  case expr of
    GHC.Case e v t as ->
      let (argExpr, _, (i', closureVarIds')) = coreExprToIrArgExpr' (setFunFlag env False) e pair
          tyParams = ie_typeParams env
          tyPaTypes = ie_typeParamTypes env
          wildId = LocalVarId (NodeId i' (varLetinValueType tyParams tyPaTypes v)) v
          valueType = typeToLetinValueType tyParams tyPaTypes t
          i'' =  i' + 1
          (alts', pair') = foldr (
              \(ac, aas, ae) (aps, (j, cvids)) ->
                let ac' =
                      case ac of
                        GHC.DataAlt dc -> DataAltCon (fromInteger (toInteger (GHC.dataConTag dc - 1)))
                        GHC.LitAlt l   -> LitAltCon (literalToIrLiteral l)
                        GHC.DEFAULT    -> DefaultAltCon
                    (lvis, j') = varsToIrLocalVarInfos env aas j
                    aas' = map (ilvi_localVarId . snd) lvis
                    env' = setFunPairMaybe (setArgIds (addLocalVarInfos (incClosureIndex env) lvis) aas') Nothing
                    (fb', _, p') = coreExprToIrFunBody' env' ae (Just valueType) (j', cvids)
                in  ((ac', (wildId, aas', fb')) : aps, p')
            ) ([], (i'', closureVarIds')) as
      in  (CaseFunApp argExpr alts', valueType, pair')

coreCaseToSimpleCase'_maybe :: (GHC.CoreExpr -> Maybe ValueType -> (Int, UniqSet LocalVarId) -> (a, ValueType, (Int, UniqSet LocalVarId))) -> (ArgExpr -> a -> a -> a) -> IrEnv -> GHC.CoreExpr -> (Int, UniqSet LocalVarId) -> Maybe (a, ValueType, (Int, UniqSet LocalVarId))
coreCaseToSimpleCase'_maybe f g env expr pair =
  case expr of
    GHC.Case e v t as ->
      let tyParams = ie_typeParams env
          tyPaTypes = ie_typeParamTypes env
          valueType = typeToLetinValueType tyParams tyPaTypes t
          case_maybe as h1 h2 =
            case as of
              [(_, [], ae1)]                            ->
                Just (h1 ae1 Nothing pair)
              [(_, [], ae1), (GHC.LitAlt l, [], ae2)]   ->
                case l of
                  GHC.MachChar '\0'  -> Just (h2 ae1 ae2 pair)
                  GHC.MachNullAddr   -> Just (h2 ae1 ae2 pair)
                  GHC.MachInt 0      -> Just (h2 ae1 ae2 pair)
                  GHC.MachInt64 0    -> Just (h2 ae1 ae2 pair)
                  GHC.MachWord 0     -> Just (h2 ae1 ae2 pair)
                  GHC.MachWord64 0   -> Just (h2 ae1 ae2 pair)
                  GHC.LitInteger 0 _ -> Just (h2 ae1 ae2 pair)
                  _                  -> Nothing
              [(_, [], ae1), (GHC.DataAlt dc, [], ae2)] ->
                if GHC.dataConTag dc == 1 then Just (h2 ae1 ae2 pair) else Nothing
              _                                         ->
                Nothing
          h altExpr1 altExpr2 pair =
            case coreExprToIrArgExpr' env expr pair of
              (e, vt, p) ->
                let (ae1', _, p') = f altExpr1 (Just valueType) p
                    (ae2', _, p'') = f altExpr2 (Just valueType) p'
                in  (g e ae1' ae2', valueType, p'')
      in  case case_maybe as f h of
            Nothing -> case_maybe (reverse as) f h
            x       -> x
    _                 -> Nothing

coreBindsToIrLocalVarInfos :: IrEnv -> [GHC.CoreBind] -> Int -> ([(GHC.Var, IrLocalVarInfo)], Int)
coreBindsToIrLocalVarInfos = coreBindsToIrLocalVarInfos' False

coreBindsToIrLocalVarInfos' :: Bool -> IrEnv -> [GHC.CoreBind] -> Int -> ([(GHC.Var, IrLocalVarInfo)], Int)
coreBindsToIrLocalVarInfos' rec env binds i = foldr (addCoreBindIrLocalVarInfos' rec env) ([], i) binds

addCoreBindIrLocalVarInfos' :: Bool -> IrEnv -> GHC.CoreBind -> ([(GHC.Var, IrLocalVarInfo)], Int) -> ([(GHC.Var, IrLocalVarInfo)], Int)
addCoreBindIrLocalVarInfos' rec env bind (infos, i) =
  case bind of
    GHC.NonRec v e ->
      let tyParams = ie_typeParams env
          tyPaTypes = ie_typeParamTypes env
          id = LocalVarId (NodeId i (varLetinValueType tyParams tyPaTypes v)) v
      in  ((v, IrLocalVarInfo id (ie_closureIndex env) rec) : infos, i + 1)
    GHC.Rec ps     ->
      let (newInfos, i') = coreBindsToIrLocalVarInfos' True env (map (\(v, lvi) -> GHC.NonRec v lvi) ps) i
      in  (newInfos ++ infos, i')

varsToIrLocalVarInfos :: IrEnv -> [GHC.Var] -> Int -> ([(GHC.Var, IrLocalVarInfo)], Int)
varsToIrLocalVarInfos env vars i = foldr (addVarIrLocalVarInfo env) ([], i) vars

addVarIrLocalVarInfo :: IrEnv -> GHC.Var -> ([(GHC.Var, IrLocalVarInfo)], Int) -> ([(GHC.Var, IrLocalVarInfo)], Int)
addVarIrLocalVarInfo env var (infos, i) =
  let tyParams = ie_typeParams env
      tyPaTypes = ie_typeParamTypes env
      id = LocalVarId (NodeId i (varLetinValueType tyParams tyPaTypes var)) var
  in  ((var, IrLocalVarInfo id (ie_closureIndex env) False) : infos, i + 1)

coreBindsToCoreExprs :: [GHC.CoreBind] -> [(GHC.Var, GHC.CoreExpr)]
coreBindsToCoreExprs binds = concatMap coreBindToCoreExprs binds

coreBindToCoreExprs :: GHC.CoreBind -> [(GHC.Var, GHC.CoreExpr)]
coreBindToCoreExprs bind =
  case bind of
    GHC.NonRec v e -> [(v, e)]
    GHC.Rec ps     -> ps

boxOrUnboxIrLetExpr :: LetExpr -> ValueType -> ValueType -> Int -> (LetExpr, Int)
boxOrUnboxIrLetExpr expr expectValueType actualValueType = undefined

boxOrUnboxIrArgExpr :: ArgExpr -> ValueType -> ValueType -> Int -> (ArgExpr, Int)
boxOrUnboxIrArgExpr expr expectValueType actualValueType = undefined

literalToIrLiteral :: GHC.Literal -> Literal
literalToIrLiteral = fst . literalToIrLiteralWithLetinValueType

literalToIrLiteralWithLetinValueType :: GHC.Literal -> (Literal, ValueType)
literalToIrLiteralWithLetinValueType lit =
  case lit of
    GHC.MachChar x      -> (Int (fromInteger (toInteger (ord x))), ValueTypeInt)
    GHC.MachStr x       -> (String x, ValueTypeRef)
    GHC.MachNullAddr    -> (Int 0, ValueTypeInt)
    GHC.MachInt x       -> (Int (fromInteger x), ValueTypeInt)
    GHC.MachInt64 x     -> (Int (fromInteger x), ValueTypeInt)
    GHC.MachWord x      -> (Int (fromInteger x), ValueTypeInt)
    GHC.MachWord64 x    -> (Int (fromInteger x), ValueTypeInt)
    GHC.MachFloat x     -> (Float (fromRational x), ValueTypeFloat)
    GHC.MachDouble x    -> (Float (fromRational x), ValueTypeFloat)
    GHC.MachLabel _ _ _ -> (Int 0, ValueTypeInt)
    GHC.LitInteger x _  -> (Int (fromInteger x), ValueTypeInt)

tyConValueTypes :: UniqFM ValueType
tyConValueTypes =
  listToUFM [
    (GHC.charPrimTyCon, ValueTypeInt),
    (GHC.intPrimTyCon, ValueTypeInt),
    (GHC.int32PrimTyCon, ValueTypeInt),
    (GHC.int64PrimTyCon, ValueTypeInt),
    (GHC.wordPrimTyCon, ValueTypeInt),
    (GHC.word32PrimTyCon, ValueTypeInt),
    (GHC.word64PrimTyCon, ValueTypeInt),
    (GHC.addrPrimTyCon, ValueTypeInt),
    (GHC.floatPrimTyCon, ValueTypeFloat),
    (GHC.doublePrimTyCon, ValueTypeFloat)]

typeToIrFunType :: GHC.Type -> FunType
typeToIrFunType typ = typeToIrFunType' typ emptyUFM (0, [])

typeToIrFunType' :: GHC.Type -> UniqFM Int -> (Int, [ArgType]) -> FunType
typeToIrFunType' typ tyParams (tyParamCount, argTypes) =
  let (tyVars, typ') = GHC.splitForAllTys typ
      tyParamCount' = tyParamCount + length tyVars
      tyParams' = addListToUFM tyParams (zip tyVars [tyParamCount .. tyParamCount'])
  in  case GHC.splitFunTy_maybe typ' of
        Just (at, rt) ->
          let argTypes' = (typeToIrArgType tyParams' at) : argTypes
          in  typeToIrFunType' rt tyParams' (tyParamCount', argTypes')
        Nothing       ->
          FunType {
            ft_typeParamCount = tyParamCount',
            ft_argTypes = reverse argTypes,
            ft_retType = (typeToIrArgType tyParams' typ')
          }

typeToIrArgType :: UniqFM Int -> GHC.Type -> ArgType
typeToIrArgType tyParams typ =
  case GHC.getTyVar_maybe typ of
    Just tv -> tyVarIrArgType tyParams tv
    Nothing ->
      case GHC.tyConAppTyCon_maybe typ of
        Just tc ->
          case lookupUFM tyConValueTypes tc of
            Just vt -> ValueType vt
            Nothing ->
              if GHC.isEnumerationTyCon tc then
                 ValueType ValueTypeInt
              else
                case GHC.tyConDataCons tc of
                  [dc] ->
                    case GHC.dataConSig dc of
                      (_, _, [t], _) -> typeToIrArgType tyParams t
                  _    -> ValueType ValueTypeRef
        Nothing -> ValueType ValueTypeRef

typeToLetinValueType :: UniqFM Int -> Array Int ValueType -> GHC.Type -> ValueType
typeToLetinValueType tyParams tyPaTypes typ = instArgType tyPaTypes (typeToIrArgType tyParams typ)

tyVarIrArgType :: UniqFM Int -> GHC.TyVar -> ArgType
tyVarIrArgType tyParams tyVar = maybe (ValueType ValueTypeRef) TypeParam (lookupUFM tyParams tyVar)

tyVarLetinValueType :: UniqFM Int -> Array Int ValueType -> GHC.TyVar -> ValueType
tyVarLetinValueType tyParams tyPaTypes tyVar = instArgType tyPaTypes (tyVarIrArgType tyParams tyVar)

varIrFunType :: GHC.Var -> FunType
varIrFunType var = typeToIrFunType (GHC.varType var)

varIrArgType :: UniqFM Int -> GHC.Var -> ArgType
varIrArgType tyParams var = typeToIrArgType tyParams (GHC.varType var)

varIrRetType :: UniqFM Int -> GHC.Var -> RetType
varIrRetType = varIrArgType

varLetinValueType :: UniqFM Int -> Array Int ValueType -> GHC.Var -> ValueType
varLetinValueType tyParams tyPaTypes var = instArgType tyPaTypes (varIrArgType tyParams var)

isInstFunVar :: UniqSet FunName -> GHC.Var -> Int-> [ValueType] -> Bool
isInstFunVar funNames var argCount valueTypes = undefined

varToGlobalVarId :: UniqFM Int -> GHC.Var -> GlobalVarId
varToGlobalVarId tyParams var =
  let fs = GHC.occNameFS (GHC.nameOccName (Var.varName var))
  in  GlobalVarId fs (varIrFunType var) var

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
