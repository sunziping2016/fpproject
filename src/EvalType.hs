-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
import Text.Printf
import qualified Data.Map.Internal as M
import System.IO

data Context = Context {t :: Maybe Type} 
  deriving (Show,Eq)

type ContextState a = StateT Context Maybe a 


t0 = EBoolLit False
t1 = EIntLit 1
t2 = ECharLit 'a'
makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
makeFun (fn, rt) ((p, t):pts) body =
  let helper [] = body
      helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
      ts = map snd pts ++ [rt]
  in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

callFun :: Expr -> [Expr] -> Expr
callFun f [e] = EApply f e
callFun f (e:es) = callFun (EApply f e) es
-- expr_even = ELambda ("x",TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))
-- t3 =   Program [] $
-- makeFun ("sum3", TInt) [("x1", TInt), ("x2", TInt), ("x3", TInt)]
-- (
--   EAdd (EAdd (EVar "x1") (EVar "x2")) (EVar "x3")
-- ) $

-- ELet ("f", callFun (EVar "sum3") [EIntLit 1, EIntLit 1]) $
-- EApply (EVar "f") (EIntLit 1)


isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval [] e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
  et <- eval [] e
  case et of
    TInt -> return TInt
    _ -> lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
  et <- eval [] e
  case et of
    TChar -> return TChar
    _ -> lift Nothing

eval :: [(String, Type)] -> Expr -> ContextState Type
eval env (EBoolLit _) = return TBool
eval env (EIntLit _) = return TInt
eval env (ECharLit _) = return TChar
eval env (ENot e) = isBool e >> return TBool

eval env (EAnd x y) = do  --
  getX <- eval env x
  if getX == TBool then do
    getY <- eval env y
    if getY == TBool then
      return TBool
    else lift Nothing
  else lift Nothing

eval env (EOr x y) = do  --
  getX <- eval env x
  if getX == TBool then do
    getY <- eval env y
    if getY == TBool then
      return TBool
    else lift Nothing
  else lift Nothing

eval env (EAdd x y) = do  --
  getX <- eval env x
  if getX == TInt then do
    getY <- eval env y
    if getY == TInt then
      return TInt
    else lift Nothing
  else lift Nothing
    
eval env (ESub x y) = do  --
  getX <- eval env x
  if getX == TInt then do
    getY <- eval env y
    if getY == TInt then
      return TInt
    else lift Nothing
  else lift Nothing
  
eval env (EMul x y) = do  --
  getX <- eval env x
  if getX == TInt then do
    getY <- eval env y
    if getY == TInt then
      return TInt
    else lift Nothing
  else lift Nothing

eval env (EDiv x y) = do  --
  getX <- eval env x
  if getX == TInt then do
    getY <- eval env y
    if getY == TInt then
      return TInt
    else lift Nothing
  else lift Nothing

eval env (EMod x y) = do  --
  getX <- eval env x
  if getX == TInt then do
    getY <- eval env y
    if getY == TInt then
      return TInt
    else lift Nothing
  else lift Nothing

eval env (EEq x y) = do  --
  getX <- eval env x
  getY <- eval env y
  if getX == getY then return TBool
  else lift Nothing

eval env (ENeq x y) = do  --
  getX <- eval env x
  getY <- eval env y
  if getX == getY then return TBool
  else lift Nothing

eval env (ELt x y) = do --
  getX <- eval env x
  getY <- eval env y
  if getX == getY then 
      if getX == TBool then 
          lift Nothing
      else 
          return TBool
  else 
      lift Nothing

eval env (EGt x y) = do --
  getX <- eval env x
  getY <- eval env y
  if getX == getY then 
      if getX == TBool then 
          lift Nothing
      else 
          return TBool
  else 
      lift Nothing

eval env (ELe x y) = do --
  getX <- eval env x
  getY <- eval env y
  if getX == getY then 
      if getX == TBool then 
          lift Nothing
      else 
          return TBool
  else 
      lift Nothing

eval env (EGe x y) = do --
  getX <- eval env x
  getY <- eval env y
  if getX == getY then 
      if getX == TBool then 
          lift Nothing
      else 
          return TBool
  else 
      lift Nothing

eval env (EIf z x y) = do --
  getX <- eval env x
  getY <- eval env y
  getZ <- eval env z
  if getZ /= TBool then 
      lift Nothing
  else
    if getX == getY then 
        return getX
    else 
        lift Nothing
     
        

-- withVar
-- withVar x t op = do
--   env <- get
--   modify $ insert x t
--   r <- op
--   put env
--   return r

eval env (ELambda (name, x) y) = do --
  getY <- eval ((name, x) : env) y
  return (TArrow x getY)

eval env (EVar name) = do
  case (M.fromList env M.!? name) of
    Just ty -> return ty
    _ -> lift Nothing

eval env (ELet (name, x) y) = do
  xtype <- eval env x
  getY <- eval ((name, xtype) : env) y
  return getY

eval env (ELetRec f (x, tx) (y, ty) e) = do
  ytype <- eval ((x, tx) : (f ,(TArrow tx ty)) : env) y
  getE <- eval ((x, tx) : (f ,(TArrow tx ty)) : env) e
  return ytype

eval env (EApply x y) = do
  (TArrow t0 t1) <- eval env x
  getY <- eval env y
  if t0 == getY then  
      return t1
  else
      lift Nothing
-- @let f = ((\x -> e1) :: tx -> ty) in e2@。
-- fromList [('a', TInt), ('b', TBool)] !? 'a' == TInt
-- fromList [(5, 'a'), (3, 'b')] !? 5 == Just 'a'  吧ELambda等中的绑定关系保存到列表当中，使用!?进行搜索
-- 比如 fromList[('Ha', TBool)] !? 'Ha' = TBool
  


-- ... more 
-- eval _ = undefined

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval [] body) $
  Context {} 
