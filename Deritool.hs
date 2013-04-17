module Deritool where

import Interface
import Data.List as DataList
import System.Process
import System.IO.Strict as Strict
import System.IO
import Data.String.Utils
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder

deriComp = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "diffWin.glade"
    
    diffWindow <- builderGetObject builder castToWindow "diffWin"
    func <- builderGetObject builder castToEntry "func"
    der <- builderGetObject builder castToEntry "der"
    val <- builderGetObject builder castToEntry "val"
    res <- builderGetObject builder castToEntry "res"
    deriv <- builderGetObject builder castToButton "deriv"
    compute <- builderGetObject builder castToButton "compute"
    
    afterClicked deriv $ do
        f <- entryGetText func
        (result,updatedVariables,updatedImports) <- getResult ("D.deriv (" ++ f ++ ")" ) [] ["import Differential as D"]
        entrySetText der (read result :: String)
        
    afterClicked compute $ do
        f <- entryGetText func
        v <- entryGetText val
        (result,updatedVariables,updatedImports) <- getResult ("D.diff (" ++ f ++ ") " ++ v ) [] ["import Differential as D"]
        entrySetText res result
    
    onDestroy diffWindow mainQuit
    widgetShowAll diffWindow
    mainGUI
