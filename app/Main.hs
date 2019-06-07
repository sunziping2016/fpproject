module Main where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Parser

main :: IO ()
main = do
    forever $ do
        line <- getLine
        parseTest (space *> pExpr <* eof) line
