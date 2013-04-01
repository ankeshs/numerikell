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
    exY <- builderGetObject builder castToButton "button1"
    exN <- builderGetObject builder castToButton "button2"
    terminal <- builderGetObject builder castToTextView "terminal"
    buf <- textViewGetBuffer terminal
    textBufferSetText buf ">> "
    getText <- builderGetObject builder castToButton "button3"
    onClicked getText $ do	 
	 ccnt <- textBufferGetCharCount buf
	 si <- textBufferGetStartIter buf
	 ei <- textBufferGetEndIter buf
	 str <- textBufferGetText buf si ei True
	 print ccnt
	 putStrLn str     
	 widgetShowAll exitDialog
    onDestroy appWindow mainQuit
    onClicked exY $ do
	 widgetHideAll exitDialog
    widgetShowAll appWindow
    mainGUI