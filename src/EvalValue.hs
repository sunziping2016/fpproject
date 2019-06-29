module EvalValue
( evalValue
) where

import AST
import Control.Monad.State
import Data.List
import qualified Data.Map.Strict as M
import Prelude hiding (getChar)

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VClosure Context String Expr
  | VFun String Context String Expr
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
    VAdtFun n vs i ->
      return $ VAdtFun n (vs ++ [p]) (i - 1)
    _ -> lift Nothing
eval (ECase e0 pes) = do
  let (ps, es) = unzip pes
  v0 <- eval e0
  let matches = map (\p -> matchPattern p v0) ps
  index <- lift $ findIndex (/= Nothing) matches
  match <- lift $ matches !! index
  withVars match $ eval (es !! index)

withAltCtx :: Context -> ContextState a -> ContextState a
withAltCtx c op = do
  env <- get
  put c
  r <- op
  put env
  return r

withVar :: String -> Value -> ContextState a -> ContextState a
withVar n v op = do
  env <- get
  modify $ \x -> x { getVars = M.insert n v $ getVars x }
  r <- op
  put env
  return r

withVars :: M.Map String Value -> ContextState a -> ContextState a
withVars vars op = do
  env <- get
  modify $ \x -> x { getVars = M.union vars $ getVars x }
  r <- op
  put env
  return r

findVar :: String -> ContextState Value
findVar n = do
  env <- get
  lift $ M.lookup n (getVars env)

getADTCtors :: [ADT] -> M.Map String Value
getADTCtors adts = M.fromList $ concat $ map adtToMap adts
  where adtToMap (ADT name ctors) = map (ctorToMap name) ctors
        ctorToMap name (param, types) = (param, VAdtFun param [] (length types))

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context { getVars = getADTCtors adts }

matchPattern :: Pattern -> Value -> Maybe (M.Map String Value)
matchPattern p v = case p of
  PBoolLit b1 -> case v of
    VBool b2 -> if b1 == b2 then Just M.empty else Nothing
    _ -> Nothing
  PIntLit i1 -> case v of
    VInt i2 -> if i1 == i2 then Just M.empty else Nothing
    _ -> Nothing
  PCharLit c1 -> case v of
    VChar c2 -> if c1 == c2 then Just M.empty else Nothing
    _ -> Nothing
  PVar x -> Just $ M.singleton x v
  PData n1 patterns -> case v of
    VAdtFun n2 values 0 -> do
      when (n1 /= n2) Nothing
      when (length patterns /= length values) Nothing
      matches <- zipWithM (\p t -> matchPattern p t) patterns values
      return $ foldl (flip M.union) M.empty matches
    _ -> Nothing

evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
