-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue
( evalValue
) where

import AST
import Control.Monad.State
import qualified Data.Map.Strict as M
import Prelude hiding (getChar)
import Debug.Trace

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VClosure Context String Expr
  | VFun String Context String Expr
  | VAdt String [Value]
  | VAdtFun String [Value] Int
  deriving (Show, Eq)

type ValueMap = M.Map String Value

data Context = Context { getVars :: ValueMap } deriving (Show, Eq)
type ContextState = StateT Context Maybe

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
  case ev of
    VInt i -> return i
    _ -> lift Nothing

getChar :: Expr -> ContextState Char
getChar e = do
  ev <- eval e
  case ev of
    VChar c -> return c
    _ -> lift Nothing

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit i) = return $ VInt i
eval (ECharLit c) = return $ VChar c
eval (ENot e) = do
  b <- getBool e
  return (VBool $ not b)
eval (EAnd e1 e2) = do
  b1 <- getBool e1
  if b1 then do
    b2 <- getBool e2
    return $ VBool b2
  else return $ VBool False
eval (EOr e1 e2) = do
  b1 <- getBool e1
  if not b1 then do
    b2 <- getBool e2
    return $ VBool b2
  else return $ VBool True
eval (EAdd e1 e2) = do
  i1 <- getInt e1
  i2 <- getInt e2
  return (VInt $ i1 + i2)
eval (ESub e1 e2) = do
  i1 <- getInt e1
  i2 <- getInt e2
  return (VInt $ i1 - i2)
eval (EMul e1 e2) = do
  i1 <- getInt e1
  i2 <- getInt e2
  return (VInt $ i1 * i2)
eval (EDiv e1 e2) = do
  i1 <- getInt e1
  i2 <- getInt e2
  case i2 of
    0 -> lift Nothing
    _ -> return (VInt $ i1 `div` i2)
eval (EMod e1 e2) = do
  i1 <- getInt e1
  i2 <- getInt e2
  case i2 of
    0 -> lift Nothing
    _ -> return (VInt $ i1 `mod` i2)
eval (EEq e1 e2) = do
  ev <- eval e1
  case ev of
    VBool x -> do b1 <- return x
                  b2 <- getBool e2
                  return (VBool $ b1 == b2)
    VInt x -> do i1 <- return x
                 i2 <- getInt e2
                 return (VBool $ i1 == i2)
    VChar x -> do c1 <- return x
                  c2 <- getChar e2
                  return (VBool $ c1 == c2)
    _ -> lift Nothing
eval (ENeq e1 e2) = do
  ev <- eval e1
  case ev of
    VBool x -> do b1 <- return x
                  b2 <- getBool e2
                  return (VBool $ b1 /= b2)
    VInt x -> do i1 <- return x
                 i2 <- getInt e2
                 return (VBool $ i1 /= i2)
    VChar x -> do c1 <- return x
                  c2 <- getChar e2
                  return (VBool $ c1 /= c2)
eval (ELt e1 e2) = do
  ev <- eval e1
  case ev of
    VInt x -> do i1 <- return x
                 i2 <- getInt e2
                 return (VBool $ i1 < i2)
    VChar x -> do c1 <- return x
                  c2 <- getChar e2
                  return (VBool $ c1 < c2)
eval (EGt e1 e2) = do
  ev <- eval e1
  case ev of
    VInt x -> do i1 <- return x
                 i2 <- getInt e2
                 return (VBool $ i1 > i2)
    VChar x -> do c1 <- return x
                  c2 <- getChar e2
                  return (VBool $ c1 > c2)
eval (ELe e1 e2) = do
  ev <- eval e1
  case ev of
    VInt x -> do i1 <- return x
                 i2 <- getInt e2
                 return (VBool $ i1 <= i2)
    VChar x -> do c1 <- return x
                  c2 <- getChar e2
                  return (VBool $ c1 <= c2)
eval (EGe e1 e2) = do
  ev <- eval e1
  case ev of
    VInt x -> do i1 <- return x
                 i2 <- getInt e2
                 return (VBool $ i1 >= i2)
    VChar x -> do c1 <- return x
                  c2 <- getChar e2
                  return (VBool $ c1 >= c2)
eval (EIf e1 e2 e3) = do
  cond <- getBool e1
  case cond of
    True -> eval e2
    False -> eval e3
eval (ELambda (pn, pt) e) = do
  env <- get
  return $ VClosure env pn e
eval (ELet (n, e1) e2) = do
  v1 <- eval e1
  withVar n v1 $ eval e2
eval (ELetRec f (x, tx) (y, ty) e) = do
  env <- get
  let fun = VFun f env x y
  withVar f fun $ eval e
eval (EVar n) = findVar n
eval (EApply e1 e2) = do
  f <- eval e1
  p <- eval e2
  case f of
    VClosure env n b ->
      withAltCtx (Context . M.insert n p . getVars $ env) $ eval b
    VFun nf env np b ->
      withAltCtx (Context . M.insert np p . M.insert nf f. getVars $ env) $ eval b
    _ -> lift Nothing
-- ... more
eval _ = undefined

withAltCtx :: Context -> ContextState a -> ContextState a
withAltCtx c op = do
  env <- get
  put c
  r <- op
  put env
  return r

withVar :: String -> Value -> ContextState a -> ContextState a
withVar n v op = do
  env <- get --save current state
  modify $ Context . M.insert n v . getVars
  r <- op
  put env -- recover state
  return r

findVar :: String -> ContextState Value
findVar n = do
  env <- get
  lift $ M.lookup n (getVars env)

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (trace ("Program:" ++ show body) $ eval body) $
  Context { getVars = M.empty } -- 可以用某种方式定义上下文，用于记录变量绑定状态

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
