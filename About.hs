module About where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder

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