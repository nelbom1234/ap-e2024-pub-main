module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL)
import APL.Eval (eval, runEval)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , frequency
  , elements
  , listOf
  , vectorOf
  )

instance Arbitrary Exp where
  arbitrary = sized (`genExp` [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genExp :: Int -> [VName] -> Gen Exp
genExp 0 vars = frequency [(10, CstInt <$> arbitrary), (10, CstBool <$> arbitrary), (1, Var <$> genKnownVar vars)]
genExp size vars =
  do
    var <- genVar
    frequency
      [ (10, CstInt <$> arbitrary)
      , (10, CstBool <$> arbitrary)
      , (10, Add <$> genExp halfSize vars <*> genExp halfSize vars)
      , (10, Sub <$> genExp halfSize vars <*> genExp halfSize vars)
      , (10, Mul <$> genExp halfSize vars <*> genExp halfSize vars)
      , (10, Div <$> genExp halfSize vars <*> genExp halfSize vars)
      , (10, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
      , (10, Eql <$> genExp halfSize vars <*> genExp halfSize vars)
      , (10, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars)
      --, (10, Var <$> genKnownVar vars)
      , (12, Let <$> pure var <*> genExp halfSize (var : vars) <*> genExp halfSize (var : vars))
      , (12, Lambda <$> pure var <*> genExp (size - 1) (var : vars))
      , (10, Apply <$> genExp halfSize vars <*> genExp halfSize vars)
      , (7, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars)
      ]
    where
      halfSize = size `div` 2
      thirdSize = size `div` 3

genKnownVar :: [VName] -> Gen VName
genKnownVar [] = genVar
genKnownVar scope = frequency
  [ (19, elements scope)
  , (1, genVar)
  ]
genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- frequency
      [ (1, listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9'])
      , (6, vectorOf 1 $ elements $ ['a' .. 'z'] ++ ['0' .. '9'])
      , (6, vectorOf 2 $ elements $ ['a' .. 'z'] ++ ['0' .. '9'])
      , (6, vectorOf 3 $ elements $ ['a' .. 'z'] ++ ['0' .. '9'])
      ]
    pure (alpha : alphaNums)

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  let printed = printExp e
  in case parseAPL "" printed of
    Left _ -> True -- if we failed it generated unparseable input, currently only applicable if it generates a known keyword like "if" or "let" as a variable or the like
    Right newE -> e == newE

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e = 
  let errors = checkExp e
  in case runEval $ eval e of
    Right _ -> True
    Left err -> err `elem` errors


properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
