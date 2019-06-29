{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import System.Timeout(timeout)

import AST
import EvalValue
import EvalType

addTimeout test = timeout (3*10^6) test >>= assertJustVerbose "3 seconds timeout exceeded"
testsWithTimeouts = wrap addTimeout htf_thisModulesTests
main = htfMain testsWithTimeouts

tRaw_01_CaseBoolBadType =
  Program [] $
  ECase (EBoolLit True) [
    (PBoolLit True, EIntLit 1),
    (PIntLit 0, EIntLit 2)
  ]
tRaw_01_CaseBoolBadType_type = Nothing

tRaw_02_CaseBoolBadType =
  Program [] $
  ECase (EBoolLit True) [
    (PBoolLit True, EIntLit 1),
    (PBoolLit False, EBoolLit False)
  ]
tRaw_02_CaseBoolBadType_type = Nothing

tRaw_03_CaseBool =
  Program [] $
  ECase (EBoolLit False) [
    (PBoolLit True, EIntLit 1),
    (PBoolLit False, EIntLit 2),
    (PBoolLit False, EIntLit 3)
  ]
tRaw_03_CaseBool_type = Just TInt
tRaw_03_CaseBool_value = RInt 2 

tRaw_04_CaseBool =
  Program [] $
  ECase (EBoolLit True) [
    (PBoolLit True, EBoolLit False),
    (PVar "x", ENot $ ENot $ EVar "x")
  ]
tRaw_04_CaseBool_type = Just TBool
tRaw_04_CaseBool_value = RBool False

test_01_CaseBoolBadType_type = assertEqual tRaw_01_CaseBoolBadType_type (evalType tRaw_01_CaseBoolBadType)
test_02_CaseBoolBadType_type = assertEqual tRaw_02_CaseBoolBadType_type (evalType tRaw_02_CaseBoolBadType)

test_03_CaseBool_type = assertEqual tRaw_03_CaseBool_type (evalType tRaw_03_CaseBool)
test_03_CaseBool_value = assertEqual tRaw_03_CaseBool_value (evalValue tRaw_03_CaseBool)
test_04_CaseBool_type = assertEqual tRaw_04_CaseBool_type (evalType tRaw_04_CaseBool)
test_04_CaseBool_value = assertEqual tRaw_04_CaseBool_value (evalValue tRaw_04_CaseBool)
