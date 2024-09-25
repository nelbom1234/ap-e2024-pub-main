module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "functions"
        [ parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTest "x (y + z)" $ Apply (Var "x") (Add (Var "y") (Var "z")),
          parserTestFail "x if x then y else z"
        ],
      testGroup
        "equality and power"
        [ parserTest "x == y" $ Eql (Var "x") (Var "y"),
          parserTest "2 == 3" $ Eql (CstInt 2) (CstInt 3),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x")),
          parserTest "x**y" $ Pow (Var "x") (Var "y"),
          parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z"))
        ],
      testGroup
        "printing"
        [ parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTest "print \"bar\" 3" $ Print "bar" (CstInt 3)
        ],
      testGroup
        "Map"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y")
        ],
      testGroup
        "pLExp"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTestFail "let true = y in z",
          parserTestFail "x let v = 2 in v",
          parserTest "\\x -> y + z" $ Lambda "x" (Add (Var "y") (Var "z")),
          parserTest "try x + y catch y - z" $ TryCatch (Add (Var "x") (Var "y")) (Sub (Var "y") (Var "z"))
        ]
    ]
