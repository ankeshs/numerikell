import Language.Haskell.Interpreter
getEval str = do
    res <-  runInterpreter (setImports ["Prelude"] >> eval str)
    return res