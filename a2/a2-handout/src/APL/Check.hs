module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import GHC.Windows (errCodeToIOError)

type Error = String

type Env = [(VName, Bool)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Bool -> Env -> Env
envExtend v b env = (v, b) : env

envLookup :: VName -> Env -> Maybe Bool
envLookup v env = lookup v env

newtype CheckM a = CheckM (Env -> Either Error a) -- TODO - give this a proper definition.

instance Functor CheckM where
    fmap = liftM

instance Applicative CheckM where
    pure x = CheckM $ \_env -> Right x
    (<*>) = ap

instance Monad CheckM where
    CheckM x >>= f = CheckM $ \env -> 
        case x env of
            Left err -> Left err
            Right x' -> 
                let (CheckM y) = f x'
                in y env

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

failure :: String -> CheckM a
failure s = CheckM $ \_env -> Left s

runCheckM :: CheckM a -> Either Error a
runCheckM (CheckM m) = m envEmpty

check :: Exp -> CheckM ()
check (CstInt _) = return ()
check (CstBool _) = return ()
check (Var v) = do
    env <- askEnv
    case envLookup v env of
        Just x -> pure ()
        Nothing -> failure $ "Variable not in scope: " ++ v
check (Add e1 e2) = checkBinOp e1 e2
check (Sub e1 e2) = checkBinOp e1 e2
check (Mul e1 e2) = checkBinOp e1 e2
check (Div e1 e2) = checkBinOp e1 e2
check (Pow e1 e2) = checkBinOp e1 e2
check (Eql e1 e2) = checkBinOp e1 e2
check (If cond e1 e2) = do
    check cond
    check e1
    check e2
check (Let var e1 e2) = do
    check e1
    localEnv (envExtend var True) $ check e2
check (Lambda var body) = do
    localEnv (envExtend var True) $ check body
check (Apply e1 e2) = do
    check e1
    check e2
check (TryCatch e1 e2) = do
    check e1
    check e2
check (Print str e) = do
    check e
check (KvPut e1 e2) = do
    check e1
    check e2
check (KvGet e) = do
    check e



checkBinOp :: Exp -> Exp -> CheckM ()
checkBinOp e1 e2 = do
    check e1
    check e2


checkExp :: Exp -> Maybe Error
checkExp e = case runCheckM $ check e of
    Left err -> Just err
    Right _ -> Nothing
