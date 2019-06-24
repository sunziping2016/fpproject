module EvalType
( evalType
) where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as M

type ValueMap = M.Map String Type
type CtorMap = M.Map String ([Type], String)

data Context = Context { getVars :: ValueMap, getCtors :: CtorMap } deriving (Show,Eq)
type ContextState a = StateT Context Maybe a 

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar
eval (ENot e) = isBool e >> return TBool

eval (EAnd x y) = do
  t1 <- eval x
  t2 <- eval y
  if t1 == TBool && t2 == TBool
    then return TBool
    else lift Nothing

eval (EOr x y) = eval (EAnd x y)

eval (EAdd x y) = do
  t1 <- eval x
  t2 <- eval y
  if t1 == TInt && t2 == TInt
    then return TInt
    else lift Nothing
    
eval (ESub x y) = eval (EAdd x y)
  
eval (EMul x y) = eval (EAdd x y)

eval (EDiv x y) = eval (EAdd x y)

eval (EMod x y) = eval (EAdd x y)

eval (EEq x y) = do
  t1 <- eval x
  t2 <- eval y
  if t1 == t2 && (t1 == TInt || t1 == TChar || t1 == TBool)
    then return TBool
    else lift Nothing

eval (ENeq x y) = eval (EEq x y)

eval (ELt x y) = do
  t1 <- eval x
  t2 <- eval y
  if t1 == t2 && (t1 == TInt || t1 == TChar)
    then return TBool
    else lift Nothing

eval (EGt x y) = eval (ELt x y)

eval (ELe x y) = eval (ELt x y)

eval (EGe x y) = eval (ELt x y)

eval (EIf x y z) = do
  t1 <- eval x
  t2 <- eval y
  t3 <- eval z
  if t1 /= TBool then 
    lift Nothing
  else
    if t2 == t3 then 
      return t2
    else 
      lift Nothing
     
eval (ELambda (n, x) y) = do
  t <- withVar n x $ eval y
  return (TArrow x t)

eval (ELet (n, x) y) = do
  t1 <- eval x
  withVar n t1 $ eval y

eval (ELetRec f (x, tx) (y, ty) e) = do
  ytype <- withVars (M.fromList [(x, tx), (f, TArrow tx ty)]) $ eval y
  if ytype == ty
    then withVar f (TArrow tx ty) $ eval e
    else lift Nothing

eval (EVar n) = findVar n

eval (EApply x y) = do
  func <- eval x
  case func of 
    TArrow t0 t1 -> do
      t2 <- eval y
      if t0 == t2  
        then return t1
        else lift Nothing
    _ -> lift Nothing

eval (ECase e0 pes) = do
  let (ps, es) = unzip pes
  env <- get
  t0 <- eval e0
  vars <- lift $ mapM (\p -> matchPattern p t0 (getCtors env)) ps
  types <- zipWithM (\v e -> withVars v $ eval e) vars es
  if and $ map (== head types) (tail types) then
    return $ head types
  else
    lift Nothing

withVar :: String -> Type -> ContextState a -> ContextState a
withVar n t op = do
  env <- get
  modify $ \x -> x { getVars = M.insert n t $ getVars x }
  r <- op
  put env
  return r

withVars :: M.Map String Type -> ContextState a -> ContextState a
withVars vars op = do
  env <- get
  modify $ \x -> x { getVars = M.union vars $ getVars x }
  r <- op
  put env
  return r
  
findVar :: String -> ContextState Type
findVar n = do
  env <- get
  lift $ M.lookup n (getVars env)

getADTTypes :: [ADT] -> M.Map String Type
getADTTypes adts = M.fromList $ concat $ map adtToFuns adts
  where adtToFuns (ADT name ctors) = map (ctorToFun name) ctors
        ctorToFun name (param, types) = (param, foldr TArrow (TData name) types)

getADTCtors :: [ADT] -> M.Map String ([Type], String)
getADTCtors adts = M.fromList $ concat $ map adtToMap adts
  where adtToMap (ADT name ctors) = map (ctorToMap name) ctors
        ctorToMap name (param, types) = (param, (types, name))

matchPattern :: Pattern -> Type -> M.Map String ([Type], String) -> Maybe (M.Map String Type)
matchPattern p t ctors = case p of
  PBoolLit _ -> case t of
    TBool -> Just M.empty
    _ -> Nothing
  PIntLit _ -> case t of
    TInt -> Just M.empty
    _ -> Nothing
  PCharLit _ -> case t of
    TChar -> Just M.empty
    _ -> Nothing
  PVar x -> Just $ M.singleton x t
  PData ctorName patterns -> case t of
    TData dataName -> do
      (types, dataName2) <- M.lookup ctorName ctors
      when (dataName /= dataName2) Nothing
      when (length patterns /= length types) Nothing
      matches <- zipWithM (\p t -> matchPattern p t ctors) patterns types
      return $ foldl (flip M.union) M.empty matches
    _ -> Nothing
  

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { getVars = getADTTypes adts, getCtors = getADTCtors adts }
