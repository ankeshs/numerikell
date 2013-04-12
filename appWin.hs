module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder
import Language.Haskell.Interpreter

main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "appWin.glade"
    
    --Binding components
    appWindow <- builderGetObject builder castToWindow "appWindow"
    exitDialog <- builderGetObject builder castToDialog "exitDialog"    
    terminal <- builderGetObject builder castToTextView "terminal"
    cmdHist <- builderGetObject builder castToTextView "cmdHist"
    varHist <- builderGetObject builder castToTextView "varHist"
    inp <- builderGetObject builder castToEntry "inp"    
    
    --Setting Properties and initializations
    textViewSetEditable terminal False
    textViewSetEditable cmdHist False
    textViewSetEditable varHist False
    buf <- textViewGetBuffer terminal
    cbuf <- textViewGetBuffer cmdHist    
    textBufferSetText buf ">> "
    ei <- textBufferGetEndIter buf
    em <- textBufferCreateMark buf Nothing ei False    
    cei <- textBufferGetEndIter cbuf
    
    --Events
    afterEntryActivate inp $ do
	 command <- entryGetText inp
	 putStrLn command
	 result <- return (getResult command)
	 entrySetText inp ""	 
	 textBufferInsert buf ei (command ++ "\n" ++ show (result) ++ "\n>> ")		 	 	 
	 scstate <- textViewScrollToMark terminal em 0.0 Nothing
	 textBufferInsert cbuf cei (command ++ "\n")
	 updateVars varHist
    onDestroy appWindow mainQuit
    
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
     