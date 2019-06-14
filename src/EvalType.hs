module EvalType
( evalType
) where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as M

type ValueMap = M.Map String Type

data Context = Context { getVars :: ValueMap } deriving (Show,Eq)
type ContextState a = StateT Context Maybe a 


t0 = EBoolLit False
t1 = EIntLit 1
t2 = ECharLit 'a'

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
  ytype <- withVars [(x, tx), (f, TArrow tx ty)] $ eval y
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

eval _ = undefined

withVar :: String -> Type -> ContextState a -> ContextState a
withVar n t op = do
  env <- get
  modify $ Context . M.insert n t . getVars
  r <- op
  put env
  return r

withVars :: [(String, Type)] -> ContextState a -> ContextState a
withVars vars op = do
  env <- get
  modify $ Context . M.union (M.fromList vars) . getVars
  r <- op
  put env
  return r
  
findVar :: String -> ContextState Type
findVar n = do
  env <- get
  lift $ M.lookup n (getVars env)


evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $
  Context { getVars = M.empty }
