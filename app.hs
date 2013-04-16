import Interface

main = do
    buf <- return []
    imports <-return []
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports1
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports1
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports
    inp <- getLine
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    putStrLn $ show buf
    putStrLn $ show result
    putStrLn $ show imports
	


