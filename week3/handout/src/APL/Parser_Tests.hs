module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Test that some string parses to the provided Exp.
parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

-- Test that some string results in a parse error. The exact error
-- message is not tested. Be careful when using this function, as it
-- is easy to make a test that fails for some reason (such as a typo)
-- than the case you are actually interested in. Generally, negative
-- tests of parsers are often not very interesting.
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
        "Constants" [ 
          parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123xyz"
        ],
      testGroup
        "Variables" [ 
          parserTest "f123" $ Var "f123",
          parserTest " f123" $ Var "f123",
          parserTest "f123 " $ Var "f123",
          parserTestFail "if"
        ],
      testGroup
        "Bools" [ 
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False,
          parserTest " true " $ CstBool True
        ],
      testGroup
        "Basic Operators" [
          parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority" [ 
          parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "if-then-else" [
          parserTest "if true then x+y else x-y" $ If (CstBool True) (Add (Var "x") (Var "y")) (Sub (Var "x") (Var "y"))
        ]
      
    ]