module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder

main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "appWin.glade"
    appWindow <- builderGetObject builder castToWindow "appWindow"
    exitDialog <- builderGetObject builder castToDialog "exitDialog"    
    terminal <- builderGetObject builder castToTextView "terminal"
    textViewSetEditable terminal False
    buf <- textViewGetBuffer terminal
    textBufferSetText buf ">> "
    inp <- builderGetObject builder castToEntry "inp"    
    afterEntryActivate inp $ do
	 command <- entryGetText inp
	 putStrLn command
	 result <- return (getResult command)
	 putStrLn result
	 entrySetText inp ""
	 ei <- textBufferGetEndIter buf
	 textBufferInsert buf ei (command ++ "\n" ++ result ++ "\n>> ")
    onDestroy appWindow mainQuit    
    widgetShowAll appWindow
    mainGUI
    
getResult :: String -> String
getResult _ = "Result unknown"