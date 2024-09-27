module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp t h)) = 
      let (psT, resT) = runEval' r s t
        in case resT of
          Right val -> (psT, Right val)
          Left _ ->
            let (psH, resH) = runEval' r s h
              in (psH, resH)
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEval' r s $ k val
        Nothing -> ([], Left $ "Key not found: " ++ show key)
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter (\(k, _) -> k /= key) s
        in runEval' r s' m
    runEval' r s (Free (TransactionOp payload m)) =
      let originalState = s
          (output, result) = runEval' r s payload
      in case result of
          Right _ -> 
            let (output', finalResult) = runEval' r s (payload >> m)
              in (output ++ output', finalResult)
          Left _ -> 
            let (output', finalResult) = runEval' r originalState m
            in (output ++ output', finalResult)
