module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
        @?= ([], Right $ ValInt 5),
      testCase "Print" $
        eval' (Print "hello" (CstInt 3))
        @?= (["hello: 3"], Right $ ValInt 3),
      testCase "div0" $
        eval' (Div (CstInt 1) (CstInt 0))
        @?= ([], Left "Division by zero")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [
      
    ]
