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
parsePADT = parse (space *> pADT <* eof) ""

tRaw_01_EBoolLit_text = "True"
tRaw_01_EBoolLit_result = Right $ EBoolLit True
tRaw_02_EBoolLit_text = "Truea"
tRaw_02_EBoolLit_result = Right $ EVar "Truea"

tRaw_01_EIntLit_text = "1"
tRaw_01_EIntLit_result = Right $ EIntLit 1
tRaw_02_EIntLit_text = "-1"
tRaw_02_EIntLit_result = Right $ EIntLit (-1)
tRaw_03_EIntLit_text = "0"
tRaw_03_EIntLit_result = Right $ EIntLit 0

tRaw_01_ECharLit_text = "'a'"
tRaw_01_ECharLit_result = Right $ ECharLit 'a'
tRaw_02_ECharLit_text = "'@'"
tRaw_02_ECharLit_result = Right $ ECharLit '@'
tRaw_03_ECharLit_text = "'1'"
tRaw_03_ECharLit_result = Right $ ECharLit '1'

tRaw_01_EAnd_text = "True && True"
tRaw_01_EAnd_result = Right $ (EAnd (EBoolLit True) (EBoolLit True))
tRaw_02_EAnd_text = "True && True && True"
tRaw_02_EAnd_result = Right $ (EAnd (EAnd (EBoolLit True) (EBoolLit True)) (EBoolLit True))
tRaw_03_EAnd_text = "(x == 1) && (y == 1)"
tRaw_03_EAnd_result = Right $ (EAnd (EEq (EVar "x") (EIntLit 1)) (EEq (EVar "y") (EIntLit 1)))
tRaw_01_EOr_text = "True || False"
tRaw_01_EOr_result = Right $ (EOr (EBoolLit True) (EBoolLit False))
tRaw_01_ENot_text = "!True"
tRaw_01_ENot_result = Right $ (ENot (EBoolLit True))
tRaw_01_EEq_text = "1 == 1"
tRaw_01_EEq_result = Right $ (EEq (EIntLit 1) (EIntLit 1))
tRaw_01_ENeq_text = "1 != 0"
tRaw_01_ENeq_result = Right $ (ENeq (EIntLit 1) (EIntLit 0))
tRaw_01_ELt_text = "0 < 1"
tRaw_01_ELt_result = Right $ (ELt (EIntLit 0) (EIntLit 1))
tRaw_02_ELt_text = "1 + 1 < 2 + 2"
tRaw_02_ELt_result = Right $ (ELt (EAdd (EIntLit 1) (EIntLit 1)) (EAdd (EIntLit 2) (EIntLit 2)))
tRaw_01_EGt_text = "1 > 0"
tRaw_01_EGt_result = Right $ (EGt (EIntLit 1) (EIntLit 0))
tRaw_01_ELe_text = "0 <= 1"
tRaw_01_ELe_result = Right $ (ELe (EIntLit 0) (EIntLit 1))
tRaw_01_EGe_text = "1 >= 0"
tRaw_01_EGe_result = Right $ (EGe (EIntLit 1) (EIntLit 0))

tRaw_01_EAdd_text = "1 + 1"
tRaw_01_EAdd_result = Right $ (EAdd (EIntLit 1) (EIntLit 1))
tRaw_02_EAdd_text = "1 + 2 * 3"
tRaw_02_EAdd_result = Right $ (EAdd (EIntLit 1) (EMul (EIntLit 2) (EIntLit 3)))
tRaw_01_ESub_text = "1 - 1"
tRaw_01_ESub_result = Right $ (ESub (EIntLit 1) (EIntLit 1))
tRaw_01_EMul_text = "1 * 1"
tRaw_01_EMul_result = Right $ (EMul (EIntLit 1) (EIntLit 1))
tRaw_01_EDiv_text = "1 / 1"
tRaw_01_EDiv_result = Right $ (EDiv (EIntLit 1) (EIntLit 1))

tRaw_01_EIf_text = "if True then 1 + 1 else 1 - 1"
tRaw_01_EIf_result = Right $
  (EIf (EBoolLit True)
  (EAdd (EIntLit 1) (EIntLit 1))
  (ESub (EIntLit 1) (EIntLit 1)))
tRaw_02_EIf_text = "if x != y then x + 1 else x - 1"
tRaw_02_EIf_result = Right $
  (EIf (ENeq (EVar "x") (EVar "y")))
  (EAdd (EVar "x") (EIntLit 1))
  (ESub (EVar "x") (EIntLit 1))
tRaw_01_ECase_text = "case x of 1 => 'a'; 2 => 'b'; 3 => 'c';"
tRaw_01_ECase_result = Right $
  (ECase (EVar "x") [(PIntLit 1, ECharLit 'a'), (PIntLit 2, ECharLit 'b'), (PIntLit 3, ECharLit 'c')])
tRaw_02_ECase_text = "case x of True => 'a'; False => 'b';"
tRaw_02_ECase_result = Right $
  (ECase (EVar "x") [(PBoolLit True, ECharLit 'a'), (PBoolLit False, ECharLit 'b')])
tRaw_03_ECase_text = "case x of x1 => 1; x2 => 2; x3 => 3;"
tRaw_03_ECase_result = Right $
  (ECase (EVar "x") [(PVar "x1", EIntLit 1), (PVar "x2", EIntLit 2), (PVar "x3", EIntLit 3)])
tRaw_04_ECase_text = "case x of #a => 1; #b => 2; #c d => 3;"
tRaw_04_ECase_result = Right $
  (ECase (EVar "x") [(PData "a" [], EIntLit 1), (PData "b" [], EIntLit 2), (PData "c" [PVar "d"], EIntLit 3)])

expr_Int_succ = ELambda ("x", TInt) (EAdd (EVar "x") (EIntLit 1))
expr_Int_succ_bad = ELambda ("x",TData "TBool") (EAdd (EVar "x") (EIntLit 1))
tRaw_01_ELambda_text = "\\x :: Int => x+1"
tRaw_01_ELambda_result = Right expr_Int_succ
tRaw_02_ELambda_text = "\\x :: TBool => x+1"
tRaw_02_ELambda_result = Right expr_Int_succ_bad

tRaw_01_ELet_text = "let x = 1 in x + 1"
tRaw_01_ELet_result = Right $ ELet ("x", (EIntLit 1)) (EAdd (EVar "x") (EIntLit 1))
tRaw_02_ELet_text = "let succ = (\\x :: Int => x+1) in succ $ 1"
tRaw_02_ELet_result = Right $ ELet ("succ", expr_Int_succ) (EApply (EVar "succ") (EIntLit 1))
tRaw_01_ELetRec_text = "letrec x = \\x :: Int => 1 :: Int in x"
tRaw_01_ELetRec_result = Right $ (ELetRec "x" ("x", TInt) (EIntLit 1, TInt) (EVar "x"))

tRaw_01_ADT_text = "data Student = Student Int"
tRaw_01_ADT_result = Right $ ADT "Student" [("Student", [TInt])]
tRaw_02_ADT_text = "data People = Student Int Char | Instructor Int Char"
tRaw_02_ADT_result = Right $ ADT "People" [("Student", [TInt, TChar]), ("Instructor", [TInt, TChar])]
tRaw_03_ADT_text = "data people = student Int Char | instructor Int Char"
tRaw_03_ADT_result = Right $ ADT "people" [("student", [TInt, TChar]), ("instructor", [TInt, TChar])]

test_01_EBoolLit = assertEqual (parsePExpr tRaw_01_EBoolLit_text) tRaw_01_EBoolLit_result
test_02_EBoolLit = assertEqual (parsePExpr tRaw_02_EBoolLit_text) tRaw_02_EBoolLit_result

test_01_EIntLit = assertEqual (parsePExpr tRaw_01_EIntLit_text) tRaw_01_EIntLit_result
test_02_EIntLit = assertEqual (parsePExpr tRaw_02_EIntLit_text) tRaw_02_EIntLit_result
test_03_EIntLit = assertEqual (parsePExpr tRaw_03_EIntLit_text) tRaw_03_EIntLit_result

test_01_ECharLit = assertEqual (parsePExpr tRaw_01_ECharLit_text) tRaw_01_ECharLit_result
test_02_ECharLit = assertEqual (parsePExpr tRaw_02_ECharLit_text) tRaw_02_ECharLit_result
test_03_ECharLit = assertEqual (parsePExpr tRaw_03_ECharLit_text) tRaw_03_ECharLit_result

test_01_EAnd = assertEqual (parsePExpr tRaw_01_EAnd_text) tRaw_01_EAnd_result
test_02_EAnd = assertEqual (parsePExpr tRaw_02_EAnd_text) tRaw_02_EAnd_result
test_03_EAnd = assertEqual (parsePExpr tRaw_03_EAnd_text) tRaw_03_EAnd_result
test_01_EOr = assertEqual (parsePExpr tRaw_01_EOr_text) tRaw_01_EOr_result
test_01_ENot = assertEqual (parsePExpr tRaw_01_ENot_text) tRaw_01_ENot_result
test_01_EEq = assertEqual (parsePExpr tRaw_01_EEq_text) tRaw_01_EEq_result
test_01_ENeq = assertEqual (parsePExpr tRaw_01_ENeq_text) tRaw_01_ENeq_result
test_01_ELt = assertEqual (parsePExpr tRaw_01_ELt_text) tRaw_01_ELt_result
test_02_ELt = assertEqual (parsePExpr tRaw_02_ELt_text) tRaw_02_ELt_result
test_01_EGt = assertEqual (parsePExpr tRaw_01_EGt_text) tRaw_01_EGt_result
test_01_ELe = assertEqual (parsePExpr tRaw_01_ELe_text) tRaw_01_ELe_result
test_01_EGe = assertEqual (parsePExpr tRaw_01_EGe_text) tRaw_01_EGe_result

test_01_EAdd = assertEqual (parsePExpr tRaw_01_EAdd_text) tRaw_01_EAdd_result
test_02_EAdd = assertEqual (parsePExpr tRaw_02_EAdd_text) tRaw_02_EAdd_result
test_01_ESub = assertEqual (parsePExpr tRaw_01_ESub_text) tRaw_01_ESub_result
test_01_EMul = assertEqual (parsePExpr tRaw_01_EMul_text) tRaw_01_EMul_result
test_01_EDiv = assertEqual (parsePExpr tRaw_01_EDiv_text) tRaw_01_EDiv_result

test_01_EIf = assertEqual (parsePExpr tRaw_01_EIf_text) tRaw_01_EIf_result
test_02_EIf = assertEqual (parsePExpr tRaw_02_EIf_text) tRaw_02_EIf_result
test_01_ECase = assertEqual (parsePExpr tRaw_01_ECase_text) tRaw_01_ECase_result
test_02_ECase = assertEqual (parsePExpr tRaw_02_ECase_text) tRaw_02_ECase_result
test_03_ECase = assertEqual (parsePExpr tRaw_03_ECase_text) tRaw_03_ECase_result
test_04_ECase = assertEqual (parsePExpr tRaw_04_ECase_text) tRaw_04_ECase_result
test_01_ELambda = assertEqual (parsePExpr tRaw_01_ELambda_text) tRaw_01_ELambda_result
test_02_ELambda = assertEqual (parsePExpr tRaw_02_ELambda_text) tRaw_02_ELambda_result
test_01_ELet = assertEqual (parsePExpr tRaw_01_ELet_text) tRaw_01_ELet_result
test_02_ELet = assertEqual (parsePExpr tRaw_02_ELet_text) tRaw_02_ELet_result
test_01_ELetRec = assertEqual (parsePExpr tRaw_01_ELetRec_text) tRaw_01_ELetRec_result
test_01_ADT = assertEqual (parsePADT tRaw_01_ADT_text) tRaw_01_ADT_result
test_02_ADT = assertEqual (parsePADT tRaw_02_ADT_text) tRaw_02_ADT_result
test_03_ADT = assertEqual (parsePADT tRaw_03_ADT_text) tRaw_03_ADT_result
