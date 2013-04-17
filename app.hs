import Interface
import System.Exit as System

-- A text based testing application for functionality of final GUI
main :: IO ()

main = do
    buf <- return []
    imports <-return []
        
    runInterface buf imports    

-- Call interface until Ctrl+C is pressed
runInterface :: [(String, String)] -> [String] -> IO ()
    
runInterface buf imports = do    
    inp <- getLine
    case inp of
        "" -> putStrLn "No command given"         
            
        ('@':evnt) -> do            
            case evnt of                    
                "clrvar" -> do
                    buf <- return []
                    putStrLn "Variables cleared"
                
                "clrimport" -> do
                    imports <- return []
                    putStrLn "Imports cleared"
                    
                "exit" -> System.exitSuccess
                
                "about" -> writeAbout
                
                otherwise -> putStrLn "Unknown Control Sequence"                            
            
        otherwise -> do 
            (result,dict,imports1) <- getResult inp buf imports
            buf <- return dict
            imports <- return imports1
            putStrLn $ (++) "Result = " $ result
            putStrLn $ (++) "Variables: " $ show buf    
            putStrLn $ (++) "Imports: " $ show imports
        
    runInterface buf imports

-- Write about program in plain text    
writeAbout :: IO ()

writeAbout = do
    putStrLn "** Numerikell 1.0 **\nHaskell for numerics\nhttp://ankeshs.org/numerikell\ngithub.com/ankeshs/numerikell"
    putStrLn "Credits:\n* Ankesh Kumar Singh\n* Dipendra Kumar Misra\n* Mukul Singh\n* Rishi Singh\n* Satyendra Patel"
