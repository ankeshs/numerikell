import System.Process
import System.IO.Strict as Strict
import System.IO

main = do
    --createProgram `seq` putStrLn "Creating script"
    --doProcess
    getOutput
    putStrLn "Done"    
    
doProcess = do
    outFile <- openFile "tmp/outFile.txt" WriteMode
    runProcess "runhaskell" ["proc.hs"] (Just "tmp/") Nothing Nothing (Just outFile) Nothing
    return ()

createProgram = do
    let prog = "main = do {putStrLn \"Child Process\"; putStrLn \"Hello2\";}"
    writeFile "tmp/proc.hs" prog 
    
getOutput = do
    logOf <- Strict.readFile "tmp/outFile.txt"
    putStrLn logOf