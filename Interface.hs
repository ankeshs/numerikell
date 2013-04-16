module Interface where

import Data.List as DataList
import System.Process
import System.IO.Strict as Strict
import System.IO
import Data.String.Utils

--getResult:: String -> [(String,String)] -> IO String
getResult str dict imports = do
  (var,imports1) <- createProgram str dict imports
  doProcess
  result <- getOutput
  let result1 = rstrip result
  if (result1 == "")
     then do
       result1 <- getError
       if (result1 == "")
          then do
            return (result1,dict,imports1)
          else do
            result1 <- return (unwords $ tail $ words $ rstrip result1)          
            return (result1,dict,imports1)
     else if (result1 /= "" && var /= "") 
	  then do 
	    --dict <- return ((++) dict [(var,result1)])
	    dict <- return $ updateVar dict (var,result1)
	    return (result1,dict,imports1)
	  else do 
	    dict <- return dict
	    return (result1,dict,imports1)
    
doProcess = do
    outFile <- openFile "tmp/outFile.txt" WriteMode
    errFile <- openFile "tmp/errFile.txt" WriteMode
    waitForProcess =<< runProcess "runhaskell" ["proc.hs"] (Just "tmp/") Nothing Nothing (Just outFile) (Just errFile)
    return ()

createProgram str dict imports = do
  let input = split "=" str
  if length input == 1
     then do 
       let command = lstrip $ head input
       if (startswith "import" command) 
	  then do
	    imports <- return ((++) imports [command])
	    writeFile "tmp/proc.hs" $ "main = do \n putStrLn \"\""
	    return ("",imports)
	  else if (startswith "@" command)
	    then do
	      return ("",imports)
	    else do 
	      writeFile "tmp/proc.hs" $ addImports imports ++ "main = do \n " ++ concatVars dict ++"putStrLn $ show("++ (str) ++")"
	      return ("",imports)
     else do
       writeFile "tmp/proc.hs" $ addImports imports ++ "main = do \n " ++ concatVars dict ++"putStrLn $ show("++ (last input) ++")"
       return (lstrip (rstrip (head input)),imports)

concatVars list = case list of
		       [] -> ""
		       (x,y):xs -> "let " ++ x ++ " = "++ y ++"\n " ++ concatVars xs

		       
updateVar list (var,value) = case list of
				  [] -> [(var,value)]
				  (x,y):xs -> if (x==var) then (x,value):xs else (x,y):updateVar xs (var,value)
				  
		       
addImports imports = case imports of
			  [] -> ""
			  x:xs -> x ++ "\n" ++ addImports xs
			  
getOutput = do
  logOf <- Strict.readFile "tmp/outFile.txt"
  return logOf

getError = do
  logOf <- Strict.readFile "tmp/errFile.txt"
  return logOf
