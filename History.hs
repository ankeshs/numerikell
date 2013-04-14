module History where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model
import Data.List as DataList

--Process Selection of Command from history     
--oneSelection :: ListStore String -> Model.TreeSelection ->  IO ()
oneSelection list tree inp = do
   sel <- Model.treeSelectionGetSelectedRows tree
   let s = head  (head sel)
   v <- Model.listStoreGetValue list s
   putStrLn $ "selected " ++ v
   entrySetText inp v

--Render Treeview   
renderTree treeview list = do
    Model.treeViewSetModel treeview list
    col <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle col "Command History"
    renderer <- Model.cellRendererTextNew
    Model.cellLayoutPackStart col renderer False
    Model.cellLayoutSetAttributes col renderer list
        $ \ind -> [Model.cellText := ind]
    Model.treeViewAppendColumn treeview col
    
--Limit the number of commands in history
trimCmdList list = do
    num <- listStoreGetSize list
    case (num>30) of
         True -> do
             listStoreRemove list 0
             trimCmdList list
         False -> do
             return ()

--Read saved Command History
readHistory = do
    file <- readFile "data/history.txt"
    putStrLn "Reading saved command history"
    return (lines file)

--Write command history for persistance    
saveHistory list = do
    cmds <- listStoreToList list
    file <- return (unlines cmds)    
    writeFile "data/history.txt" file
    putStrLn "Saving Command History"