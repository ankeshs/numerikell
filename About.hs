{-
        Licence Information

        This file is part of Numerikell 1.0.0.0 Haskell Numerical
        Software project. Please do not share, copy, edit or distribute
        without owner's permission.  

        @Contributors : Please striclty follow Haskell community convention.
        Comment your code, use proper nomenclature for functions, variables
        and modules.

        File Specification :
        Contributor : Ankesh Kumar Singh
-}

module About (makeAboutDialog) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder

makeAboutDialog :: IO ()

-- Make About Dialog on request

makeAboutDialog = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "appWin.glade"
    aboutDialog <- builderGetObject builder castToAboutDialog "aboutDialog"
    widgetShowAll aboutDialog
    resp <- dialogRun aboutDialog
    putStrLn $ show resp
    case resp of
         ResponseCancel -> widgetHide aboutDialog