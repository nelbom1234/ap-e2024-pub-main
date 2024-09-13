module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval, printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       ( Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )

-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
          --
          -- TODO - add more
      testCase "lambda" $
        eval envEmpty (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
        @?= Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
      testCase "apply" $
        eval envEmpty (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
        @?= Right (ValInt 5),
      --
      testCase "tryTrue" $
        eval envEmpty (TryCatch (CstInt 0) (CstInt 1))
        @?= Right (ValInt 0),
      --
      testCase "tryFalse" $
        eval envEmpty (TryCatch (Var "missing") (CstInt 1))
        @?= Right (ValInt 1),
      --  
      testCase "printInt" $
        printExp (CstInt 2)
        @?= "2",
      --
      testCase "printBool" $
        printExp (CstBool True)
        @?= "True",
      --
      testCase "printAdd" $
        printExp (Add (CstInt 1) (CstInt 2))
        @?= "(1 + 2)",
      --
      testCase "printSub" $
        printExp (Sub (CstInt 1) (CstInt 2))
        @?= "(1 - 2)",
      --
      testCase "printMul" $
        printExp (Mul (CstInt 1) (CstInt 2))
        @?= "(1 * 2)",
      --
      testCase "printDiv" $
        printExp (Div (CstInt 1) (CstInt 2))
        @?= "(1 / 2)",
      --
      testCase "printPow" $
        printExp (Pow (CstInt 1) (CstInt 2))
        @?= "(1 ** 2)",
      --
      testCase "printIfBool" $
        printExp (If (CstBool True) (CstInt 2) (CstInt 4))
        @?= "(if (True) then 2 else 4)",
      --
      testCase "printIfEql" $
        printExp (If (Eql (CstInt 4) (CstInt 4)) (CstInt 2) (CstInt 3))
        @?= "(if (4 == 4) then 2 else 3)",
      --
      testCase "printLet" $
        printExp (Let "x" (CstInt 3) (Add (Var "x") (Var "x")))
        @?= "(let x = 3 in (x + x))",
      --
      testCase "printLambda" $
        printExp (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
        @?= "(let x = 2 in (\\y -> (x + y)))",
      --
      testCase "printApply" $
        printExp (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
        @?= "((let x = 2 in (\\y -> (x + y))) 3)",
      --
      testCase "printTry" $
        printExp (TryCatch (Var "missing") (CstInt 1))
        @?= "(try (missing) catch (1))"
    ]
