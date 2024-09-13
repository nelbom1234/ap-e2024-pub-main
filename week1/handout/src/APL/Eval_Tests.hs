module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      testCase "eval" $
        eval envEmpty (CstInt 7)
        @?= Right (ValInt 7),
      --
      testCase "add" $
        eval envEmpty (Add (CstInt 5) (CstInt 3))
        @?= Right (ValInt 8),
      --
      testCase "sub" $
        eval envEmpty (Sub (CstInt 6) (CstInt 2))
        @?= Right (ValInt 4),
      --
      testCase "mul" $
        eval envEmpty (Mul (CstInt 3) (CstInt 5))
        @?= Right (ValInt 15),
      --
      testCase "divInt" $
        eval envEmpty (Div (CstInt 8) (CstInt 4))
        @?= Right (ValInt 2),
      --
      testCase "divDecimal" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
        @?= Right (ValInt 2),
      --
      testCase "pow" $
        eval envEmpty (Pow (CstInt 3) (CstInt 3))
        @?= Right (ValInt 27),
      --
      testCase "eql" $
        eval envEmpty (Eql (CstInt 5) (CstInt 5))
        @?= Right (ValBool True),
      --
      testCase "notEql" $
        eval envEmpty (Eql (CstInt 3) (CstInt 6))
        @?= Right (ValBool False),
      --
      testCase "eqlBools" $
        eval envEmpty (Eql (CstBool True) (CstBool True))
        @?= Right (ValBool True),
      --
      testCase "ife1" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Add (CstInt 4) (CstInt 5)))
        @?= Right (ValInt 2),
      --
      testCase "ife2" $
        eval envEmpty (If (CstBool False) (CstInt 2) (Add (CstInt 4) (CstInt 5)))
        @?= Right (ValInt 9),
      --
      testCase "let" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x")))
        @?= Right (ValInt 6),
      --
      testCase "let2" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y")))
        @?= Left "Unknown variable: y"
    ]
