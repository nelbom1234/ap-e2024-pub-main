module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import qualified Data.Map as Map
import Data.Map (Map)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

instance Ord Val where
  compare (ValInt x) (ValInt y) = compare x y
  compare (ValBool x) (ValBool y) = compare x y
  compare (ValFun _ _ _) (ValFun _ _ _) = EQ -- Arbitrary ordering
  compare (ValInt _) _ = LT
  compare (ValBool _) (ValFun _ _ _) = LT
  compare _ _ = GT

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

type Print = [String]

newtype EvalM a = EvalM (Env -> (Map Val Val, Print) -> (Either Error a, (Map Val Val, Print)))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env state -> (Right x, state)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env state ->
    case x env state of
      (Left err, state') -> (Left err, state')
      (Right x', state') ->
        let (EvalM y) = f x'
         in y env state'

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> (Right env, state)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state -> m (f env) state

failure :: String -> EvalM a
failure s = EvalM $ \_env state -> (Left s, state)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state ->
  case m1 env state of
    (Left _, state' ) -> m2 env state'
    (Right x, state') -> (Right x, state')

getState :: EvalM (Map Val Val, Print)
getState = EvalM $ \_env state -> (Right state, state)

putState :: (Map Val Val, Print) -> EvalM ()
putState newState = EvalM $ \_env _ -> (Right (), newState)

modifyState :: ((Map Val Val, Print) -> (Map Val Val, Print)) -> EvalM ()
modifyState f = EvalM $ \_env state -> (Right (), f state)

modifyPrint :: (Print -> Print) -> EvalM ()
modifyPrint f = modifyState (\(kv, p) -> (kv, f p))

modifyMap :: (Map Val Val -> Map Val Val) -> EvalM ()
modifyMap f = modifyState (\(kv, p) -> (f kv, p))

kvPut :: Val -> Val -> EvalM ()
kvPut k v = modifyMap (Map.insert k v)

kvGet :: Val -> EvalM (Maybe Val)
kvGet k = do
  (kvMap, _) <- getState
  return $ Map.lookup k kvMap

runEval :: EvalM a -> (Print, Either Error a)
runEval (EvalM m) = 
  let (result, (_, finalState)) = m envEmpty (Map.empty, [])
  in (finalState, result)

evalPrint :: String -> EvalM ()
evalPrint str = do
  modifyPrint (\s -> s ++ [str])
  return ()

evalKvGet :: Val -> EvalM Val
evalKvGet k = do 
  v <- kvGet k
  case v of
    Just v' -> return v'
    Nothing -> failure $ "Invalid key: " ++ show k

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut k v = kvPut k v
  

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
eval (Print str e) = do
  v1 <- eval e
  case e of
    Lambda var f -> evalPrint (str ++ ": #" ++ "(" ++ "\\" ++ var ++ " -> " ++ show f ++ ")")
    CstInt a -> evalPrint (str ++ ": " ++ show a)
    CstBool a -> evalPrint (str ++ ": " ++ show a)
    _ -> evalPrint (str ++ ": #" ++ "(" ++ show e ++ ")")
  return v1
eval (KvPut e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalKvPut v1 v2
  return v2
eval (KvGet e) = do
  v <- eval e
  evalKvGet v