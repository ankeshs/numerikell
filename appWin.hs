{-
        Licence Information

        This file is part of Numerikell 1.0.0.0 Haskell Numerical
        Software project. Please do not share, copy, edit or distribute
        without owner's permission.  

        @Contributors : Please striclty follow Haskell community convention.
        Comment your code, use proper nomenclature for functions, variables
        and modules.

        File Specification :
        Contributor : Ankesh Kumar Singh, Satyendra Patel
-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder
import About
import History
import Interface
import Plot
import Deritool
import Graphics.UI.Gtk.ModelView as Model
import Data.List as DataList
import System.Process
import System.IO.Strict as Strict
import System.IO
import Data.String.Utils
import Control.Monad

main :: IO ()

main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "appWin.glade"
    
    --Binding components
    appWindow <- builderGetObject builder castToWindow "appWindow"
    exitDialog <- builderGetObject builder castToDialog "exitDialog"    
    terminal <- builderGetObject builder castToTextView "terminal"
    cmdHist <- builderGetObject builder castToTreeView "cmdHist"     
    varHist <- builderGetObject builder castToTextView "varHist"
    impHist <- builderGetObject builder castToTextView "impHist"     
    inp <- builderGetObject builder castToEntry "inp"
    inpComp <- builderGetObject builder castToEntryCompletion "inpComp"
    exitMenu <- builderGetObject builder castToMenuItem "exitMenu"    
    viewCmds <- builderGetObject builder castToCheckMenuItem "viewCmds"
    fullScr <- builderGetObject builder castToCheckMenuItem "fullScr"
    clrCmd <- builderGetObject builder castToMenuItem "clrCmd"
    clrTer <- builderGetObject builder castToMenuItem "clrTer"
    clrVar <- builderGetObject builder castToMenuItem "clrVar"
    clrImp <- builderGetObject builder castToMenuItem "clrimp"
    varBox <- builderGetObject builder castToVBox "varBox"
    cmdBox <- builderGetObject builder castToVBox "cmdBox"    
    aboutMenu <- builderGetObject builder castToMenuItem "aboutMenu"
    plotMenu <- builderGetObject builder castToMenuItem "plotMenu"
    derMenu <- builderGetObject builder castToMenuItem "derTool"
        
    --Setting Properties and initializations    
    
    textViewSetEditable terminal False    
    textViewSetEditable varHist False
    textViewSetEditable impHist False
    buf <- textViewGetBuffer terminal    
    textBufferSetText buf ">> "
    ei <- textBufferGetEndIter buf
    em <- textBufferCreateMark buf Nothing ei False        
    cmdArray <- readHistory
    cmdList <- listStoreNew cmdArray    
    renderTree cmdHist cmdList
    cmdTree <- Model.treeViewGetSelection cmdHist
        
    Model.treeSelectionSetMode cmdTree  SelectionSingle    
    
    --Events
    afterActivateLeaf exitMenu $ do
	 putStrLn "Exit Request"
	 exitOperations cmdList
		 
    on viewCmds checkMenuItemToggled $ do
	 putStrLn "Command View Toggled"    
	 
    on fullScr checkMenuItemToggled $ do
         slctd <- checkMenuItemGetActive fullScr
         case slctd of
            True -> windowFullscreen appWindow
            False -> windowUnfullscreen appWindow  
	 
    afterActivateLeaf aboutMenu $ do
	 makeAboutDialog
    
    afterActivateLeaf plotMenu $ do
         putStrLn "Plot Requested" 
         plotComp
         
    afterActivateLeaf derMenu $ do
         putStrLn "Derivative Toolbox Requested" 
         deriComp
         
    afterActivateLeaf clrCmd $ do
         listStoreClear cmdList
    
    afterActivateLeaf clrTer $ do
        textBufferSetText buf ">> " 
        
    afterActivateLeaf clrVar $ do
        updateVariables varHist []
        
    afterActivateLeaf clrImp $ do
        updateImports impHist []
    
    afterEntryActivate inp $ do
	 command <- entryGetText inp
	 ei <- textBufferGetEndIter buf
         em <- textBufferCreateMark buf Nothing ei False
	 
	 case command of
            "" -> putStrLn "No command given"
            
            "@clear" -> do
                textBufferSetText buf ">> "
                entrySetText inp ""
                
            ('@':evnt) -> do
                textBufferInsert buf ei (command ++ "\n")
                case evnt of
                     "clrcmd" -> listStoreClear cmdList
                     "clrvar" -> updateVariables varHist []
                     "clrimport" -> updateImports impHist []                     
                     "exit" -> exitOperations cmdList
                     "about" -> makeAboutDialog
                     otherwise -> textBufferInsert buf ei ("Unknown Control Sequence\n")                
                
                textBufferInsert buf ei (">> ")
                entrySetText inp ""
                
            otherwise -> do 
		putStrLn command		
		inputString <- return command
		variables <- getVariables varHist
		imports <- getImports impHist
		
		(result,updatedVariables,updatedImports) <- getResult inputString variables imports
		variables <- return updatedVariables
		imports <- return updatedImports
		entrySetText inp ""
				
		textBufferInsert buf ei (command ++ "\n" ++ result ++ "\n>> ")		 	 	 
		scstate <- textViewScrollToMark terminal em 0.0 Nothing
		
		aVal <- listStoreAppend cmdList command
		treeViewScrollToPoint cmdHist 0 (aVal*100)
		trimCmdList cmdList
		
		updateVariables varHist	variables
		updateImports impHist imports
	 
    Model.onSelectionChanged cmdTree (oneSelection cmdList cmdTree inp)
    
    onDestroy appWindow (exitOperations cmdList)
    
    widgetShowAll appWindow
    mainGUI
    
--Perform operations before exiting the program
exitOperations :: ListStore String -> IO ()

exitOperations list = do
    saveHistory list
    mainQuit
