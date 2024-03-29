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

module History (
    oneSelection,
    renderTree,
    readHistory,
    saveHistory,
    getVariables,
    updateVariables,
    getImports,
    trimCmdList,
    updateImports) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model
import Data.List as DataList
import Data.String.Utils

--Process Selection of Command from history     
oneSelection :: 
    (TreeSelectionClass self, EntryClass self1)
    => ListStore String -> self -> self1 -> IO ()

oneSelection list tree inp = do
   sel <- Model.treeSelectionGetSelectedRows tree
   let s = head  (head sel)
   v <- Model.listStoreGetValue list s
   putStrLn $ "selected " ++ v
   entrySetText inp v

--Render Treeview   
renderTree :: 
    (TreeViewClass self, TreeModelClass (model String), TypedTreeModelClass model) 
    => self -> model String -> IO Int

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
trimCmdList :: ListStore a -> IO ()

trimCmdList list = do
    num <- listStoreGetSize list
    case (num>30) of
         True -> do
             listStoreRemove list 0
             trimCmdList list
         False -> do
             return ()

--Read saved Command History
readHistory :: IO [String]

readHistory = do
    file <- readFile "data/history.txt"
    putStrLn "Reading saved command history"
    return (lines file)

--Write command history for persistance  
saveHistory :: ListStore String -> IO ()

saveHistory list = do
    cmds <- listStoreToList list
    file <- return (unlines cmds)    
    writeFile "data/history.txt" file
    putStrLn "Saving Command History"
    
-- Obtain current set of variables in the environment    
getVariables :: TextViewClass self => self -> IO [(String, String)]
    
getVariables vh = do
     vbuf <- textViewGetBuffer vh
     vsi <- textBufferGetStartIter vbuf
     vei <- textBufferGetEndIter vbuf
     vcstr <- textBufferGetText vbuf vsi vei False
          
     varmap <- return( lines vcstr )
     return $ map getVarTuple varmap

getVarTuple :: String -> (String, String)     
getVarTuple l = (head t, last t)
    where t = split " = " l
    
-- Update the current set of variables to the environment   
updateVariables
  :: TextViewClass self => self -> [(String, String)] -> IO ()
    
updateVariables vh varmap = do    
     vbuf <- textViewGetBuffer vh
     vsi <- textBufferGetStartIter vbuf
     vei <- textBufferGetEndIter vbuf     
     textBufferSetText vbuf (unlines $ map glueVarTuple varmap)
         where glueVarTuple (a,b) = a ++ " = " ++ b     

--Get current set of imported libraries from the environment
getImports :: TextViewClass self => self -> IO [String]
         
getImports vh = do
     vbuf <- textViewGetBuffer vh
     vsi <- textBufferGetStartIter vbuf
     vei <- textBufferGetEndIter vbuf
     vcstr <- textBufferGetText vbuf vsi vei False
     
     impmap <- return( lines vcstr )
     return $ map ( (++) "import ") impmap 
     
--Update imported libraries to the environment    
updateImports :: TextViewClass self => self -> [String] -> IO ()
     
updateImports vh impmap = do
     vbuf <- textViewGetBuffer vh
     vsi <- textBufferGetStartIter vbuf
     vei <- textBufferGetEndIter vbuf     
     textBufferSetText vbuf (unlines $ map getImpName impmap)
        where getImpName str = unwords $ tail $ words str