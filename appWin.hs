module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder
import About
import History
import Graphics.UI.Gtk.ModelView as Model
import Data.List as DataList

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
    inp <- builderGetObject builder castToEntry "inp"
    exitMenu <- builderGetObject builder castToMenuItem "exitMenu"    
    clrCmd <- builderGetObject builder castToMenuItem "clrCmd"
    clrTer <- builderGetObject builder castToMenuItem "clrTer"
    varBox <- builderGetObject builder castToVBox "varBox"
    cmdBox <- builderGetObject builder castToVBox "cmdBox"    
    aboutMenu <- builderGetObject builder castToMenuItem "aboutMenu"
    plotMenu <- builderGetObject builder castToMenuItem "plotMenu"
        
    --Setting Properties and initializations
    textViewSetEditable terminal False    
    textViewSetEditable varHist False
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
		 
    afterActivateLeaf aboutMenu $ do
	 makeAboutDialog
    
    afterActivateLeaf plotMenu $ do
         putStrLn "Plot Requested"
         
    afterActivateLeaf clrCmd $ do
         listStoreClear cmdList
    
    afterActivateLeaf clrTer $ do
        textBufferSetText buf ">> "        
    
    afterEntryActivate inp $ do
	 command <- entryGetText inp
	 case command of
            "" -> putStrLn "No command given"
            otherwise -> do 
                putStrLn command
                result <- return (getResult command)
                
                entrySetText inp ""
                
                ei <- textBufferGetEndIter buf
                em <- textBufferCreateMark buf Nothing ei False
                textBufferInsert buf ei (command ++ "\n" ++ show (result) ++ "\n>> ")		 	 	 
                scstate <- textViewScrollToMark terminal em 0.0 Nothing
                
                aVal <- listStoreAppend cmdList command
                treeViewScrollToPoint cmdHist 0 (aVal*100)
                trimCmdList cmdList
                
                updateVars varHist	 	 
	 
    Model.onSelectionChanged cmdTree (oneSelection cmdList cmdTree inp)
    
    onDestroy appWindow (exitOperations cmdList)
    
    widgetShowAll appWindow
    mainGUI
    
--Takes the command entered and returns output, may additionally take environment with variables
getResult :: String -> String
getResult str = "Result Unknown"

--Code to be modified to keep track of changing variables
updateVars vh = do
     vbuf <- textViewGetBuffer vh
     vsi <- textBufferGetStartIter vbuf
     vei <- textBufferGetEndIter vbuf
     vcstr <- textBufferGetText vbuf vsi vei False
     putStrLn ("Variables:\n" ++ vcstr)
     textBufferSetText vbuf "a=10\nb=20\nc=5"

--Perform operations before exiting the program
exitOperations list = do
    saveHistory list
    mainQuit