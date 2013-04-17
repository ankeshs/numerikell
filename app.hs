{-
        Licence Information

        This file is part of Numerikell 1.0.0.0 Haskell Numerical
        Software project. Please do not share, copy, edit or distribute
        without owner's permission.  

        @Contributors : Please striclty follow Haskell community convention.
        Comment your code, use proper nomenclature for functions, variables
        and modules.

        File Specification :
        Contributor : Mukul singh, Ankesh Kumar Singh
-}

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
    
    (result,dict,imports1) <- getResult inp buf imports
    buf <- return dict
    imports <- return imports1
    
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
            putStrLn $ (++) "Result = " $ result
            putStrLn $ (++) "Variables: " $ show buf    
            putStrLn $ (++) "Imports: " $ show imports
        
    runInterface buf imports

-- Write about program in plain text    
writeAbout :: IO ()

writeAbout = do
    putStrLn "** Numerikell 1.0 **\nHaskell for numerics\nhttp://ankeshs.org/numerikell\ngithub.com/ankeshs/numerikell"
    putStrLn "Credits:\n* Ankesh Kumar Singh\n* Dipendra Kumar Misra\n* Mukul Singh\n* Rishi Singh\n* Satyendra Patel"
