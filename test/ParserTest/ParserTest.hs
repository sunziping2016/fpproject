{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import System.Timeout(timeout)

import AST
import Parser
import Text.Megaparsec
import Text.Megaparsec.Char

addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests
main = htfMain testsWithTimeouts

parsePExpr = parse (space *> pExpr <* eof) ""

tRaw_01_EBoolLit_text = "True"
tRaw_01_EBoolLit_result = Right $ EBoolLit True
tRaw_02_EBoolLit_text = "Truea"
tRaw_02_EBoolLit_result = Right $ EVar "Truea"

test_01_EBoolLit = assertEqual (parsePExpr tRaw_01_EBoolLit_text) tRaw_01_EBoolLit_result
test_02_EBoolLit = assertEqual (parsePExpr tRaw_02_EBoolLit_text) tRaw_02_EBoolLit_result
