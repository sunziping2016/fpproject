module Parser
( pExpr,
  pType,
  pPattern,
  pADT,
  Action(..),
  pAction
) where

-- from https://markkarpov.com/megaparsec/megaparsec.html

import AST
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Expression Parser

pKeyword :: String -> Parser String
pKeyword keyword = lexeme (try $ string keyword <* notFollowedBy alphaNumChar)

pVariable :: Parser String
pVariable = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

pSubExpr :: Parser Expr
pSubExpr = between (symbol "(") (symbol ")") pExpr

pBoolLitTrue :: Parser Expr
pBoolLitTrue = EBoolLit True <$ pKeyword "True"

pBoolLitFalse :: Parser Expr
pBoolLitFalse = EBoolLit False <$ pKeyword "False"

pIntLitPositive :: Parser Expr
pIntLitPositive = EIntLit <$> lexeme L.decimal

pIntLitNegative :: Parser Expr
pIntLitNegative = EIntLit . negate <$> lexeme (symbol "-" *> L.decimal)

pCharLit :: Parser Expr
pCharLit = ECharLit <$> lexeme (between (char '\'') (char '\'') L.charLiteral)

pTerm :: Parser Expr
pTerm = choice
  [ pSubExpr
  , pBoolLitTrue
  , pBoolLitFalse
  , pIntLitPositive
  , pIntLitNegative
  , pCharLit
  , pIf
  , pLambda
  , pLet
  , pLetRec
  , pCase
  , pVar ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (ENot   <$ symbol "!") ]
  , [ InfixL (EApply <$ symbol "$") ]
  , [ InfixL (EMul   <$ symbol "*")
    , InfixL (EDiv   <$ symbol "/")
    , InfixL (EMod   <$ symbol "%") ]
  , [ InfixL (EAdd   <$ symbol "+" )
    , InfixL (ESub   <$ symbol "-" ) ]
  , [ InfixL (ELe    <$ symbol "<=")
    , InfixL (EGe    <$ symbol ">=")
    , InfixL (ELt    <$ symbol "<")
    , InfixL (EGt    <$ symbol ">") ]
  , [ InfixL (EEq    <$ symbol "==")
    , InfixL (ENeq   <$ symbol "!=") ]
  , [ InfixL (EAnd   <$ symbol "&&") ]
  , [ InfixL (EOr    <$ symbol "||") ] ]

pIf :: Parser Expr
pIf = try $ do
  pKeyword "if"
  e1 <- pExpr
  pKeyword "then"
  e2 <- pExpr
  pKeyword "else"
  e3 <- pExpr
  return $ EIf e1 e2 e3

pLambda :: Parser Expr
pLambda = try $ do
  symbol "\\"
  pn <- pVariable
  symbol "::"
  pt <- pType
  symbol "=>"
  e <- pExpr
  return $ ELambda (pn, pt) e

pLet :: Parser Expr
pLet = try $ do
  pKeyword "let"
  n <- pVariable
  symbol "="
  e1 <- pExpr
  pKeyword "in"
  e2 <- pExpr
  return $ ELet (n, e1) e2

pLetRec :: Parser Expr
pLetRec = try $ do
  pKeyword "letrec"
  f <- pVariable
  symbol "="
  symbol "\\"
  x <- pVariable
  symbol "::"
  tx <- pType
  symbol "=>"
  e1 <- pExpr
  symbol "::"
  ty <- pType
  pKeyword "in"
  e2 <- pExpr
  return $ ELetRec f (x, tx) (e1, ty) e2

pVar :: Parser Expr
pVar = EVar <$> pVariable

pCase :: Parser Expr
pCase = try $ do
  pKeyword "case"
  e <- pExpr
  pKeyword "of"
  ps <- some . try $ do
    p <- pPattern
    symbol "=>"
    e <- pExpr
    symbol ";"
    return (p, e)
  return $ ECase e ps

-- Type Parser

pTypeBool :: Parser Type
pTypeBool = TBool <$ pKeyword "Bool"

pTypeInt :: Parser Type
pTypeInt = TInt <$ pKeyword "Int"

pTypeChar :: Parser Type
pTypeChar = TChar <$ pKeyword "Char"

pTypeData :: Parser Type
pTypeData = TData <$> pVariable

pTypeTerm :: Parser Type
pTypeTerm = choice
  [ pTypeBool
  , pTypeInt
  , pTypeChar
  , pTypeData]

pType :: Parser Type
pType = makeExprParser pTypeTerm typeOperatorTable

typeOperatorTable :: [[Operator Parser Type]]
typeOperatorTable =
  [ [ InfixR (TArrow <$ symbol "->") ] ]

-- Pattern Parser

pSubPattern :: Parser Pattern
pSubPattern = between (symbol "(") (symbol ")") pPattern

pPatternBoolLitTrue :: Parser Pattern
pPatternBoolLitTrue = PBoolLit True <$ pKeyword "True"

pPatternBoolLitFalse :: Parser Pattern
pPatternBoolLitFalse = PBoolLit False <$ pKeyword "False"

pPatternIntLitPositive :: Parser Pattern
pPatternIntLitPositive = PIntLit <$> lexeme L.decimal

pPatternIntLitNegative :: Parser Pattern
pPatternIntLitNegative = PIntLit . negate <$> lexeme (symbol "-" *> L.decimal)

pPatternVar :: Parser Pattern
pPatternVar = PVar <$> pVariable

pPattern :: Parser Pattern
pPattern = choice
  [ pSubPattern
  , pPatternBoolLitTrue
  , pPatternBoolLitFalse
  , pPatternIntLitPositive
  , pPatternIntLitNegative
  , pPatternData
  , pPatternVar ]

pPatternExceptData :: Parser Pattern
pPatternExceptData = choice
  [ pSubPattern
  , pPatternBoolLitTrue
  , pPatternBoolLitFalse
  , pPatternIntLitPositive
  , pPatternIntLitNegative
  , pPatternVar ]

pPatternData :: Parser Pattern
pPatternData = try $ do
  symbol "#"
  name <- pVariable
  patterns <- many pPatternExceptData
  return $ PData name patterns

-- ADT parser
pADT :: Parser ADT
pADT = try $ do
  pKeyword "data"
  t <- pVariable
  symbol "="
  ds <- (flip sepBy1) (symbol "|") . try $ do
    d <- pVariable
    ts <- many . try $ pType
    return (d, ts)
  return $ ADT t ds

-- Action Parser
data Action
  = AType Expr
  | AEval Expr
  | AADT ADT
  | APrintADT
  | AParse Expr
  deriving (Show, Eq)

pActionType :: Parser Action
pActionType = try $ do
  pKeyword ":t"
  expr <- pExpr
  return $ AType expr

pActionADT :: Parser Action
pActionADT = try $ do
  adt <- pADT
  return $ AADT adt

pActionEval :: Parser Action
pActionEval = try $ do
  expr <- pExpr
  return $ AEval expr

pActionPrintADT :: Parser Action
pActionPrintADT = try $ do
  pKeyword ":i"
  return $ APrintADT

pActionParse :: Parser Action
pActionParse = try $ do
  pKeyword ":p"
  expr <- pExpr
  return $ AParse expr

pAction :: Parser Action
pAction = choice
  [ pActionType
  , pActionADT
  , pActionEval
  , pActionPrintADT
  , pActionParse ]
