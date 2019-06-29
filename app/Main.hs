module Main where

import AST
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import EvalType (evalType)
import EvalValue (evalValue)
import Text.Megaparsec
import Text.Megaparsec.Char
import Parser
import System.IO

data Context = Context { getADTs :: M.Map String ADT } deriving (Show, Eq)
type ContextState = StateT Context IO

doREPL :: ContextState a
doREPL = do
  forever $ do
    lift $ putStr "> "
    lift $ hFlush stdout
    line <- lift getLine
    adts <- fmap (M.elems . getADTs) get
    case parse (space *> pAction <* eof) "" line of
      Left bundle -> lift $ putStr (errorBundlePretty bundle)
      Right action -> case action of
        AType expr -> lift $ do
          let program = Program adts expr
          case evalType program of
            Just t -> print t
            Nothing -> putStrLn "Error"
        AEval expr -> lift $ do
          let program = Program adts expr
          case evalValue program of
            RBool t -> print t
            RInt t -> print t
            RChar t -> print t
            RInvalid -> putStrLn "Error"
        AADT adt@(ADT name _) ->
          modify $ Context . M.insert name adt . getADTs
        APrintADT -> lift $
          mapM_ print adts
        AParse expr -> lift $
          print expr

main :: IO ()
main = evalStateT doREPL (Context { getADTs = M.empty })
