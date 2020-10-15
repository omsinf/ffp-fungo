module FileChooser (openChooser) where

import Graphics.UI.Gtk hiding (response)
import ModelSG ( GameSG )
import ParseSG ( createSGF, parseSGF )
import Control.Concurrent.MVar ( readMVar, MVar )
import Control.Monad ( when )
import Data.List ( isSuffixOf )
import Data.Either ( fromLeft, fromRight, isRight )
import ClientUtil ( safeReplaceMVar )

openChooser :: MVar GameSG -> MVar Int ->  IO ()
openChooser game navMVar = do
  initGUI

  -- load up our main window
  gui <- builderNew
  builderAddFromFile gui "app/fungo_filechooser.glade"

  mainWindow <- builderGetObject gui castToWindow "mainWindow"

  -- get a handle on a various objects from the glade file
  on mainWindow objectDestroy mainQuit

  let onClicked obj = on obj buttonActivated

  openFileButton <- builderGetObject gui castToButton "openFileButton"
  openFileButton `onClicked` openOpenFileDialog mainWindow game navMVar

  saveFileButton <- builderGetObject gui castToButton "saveFileButton"
  saveFileButton `onClicked` openSaveFileDialog mainWindow game

  quitButton <- builderGetObject gui castToButton "quitButton"
  quitButton `onClicked` (do
                           widgetDestroy mainWindow
                           mainQuit)

  -- The final step is to display the main window and run the main loop
  widgetShowAll mainWindow
  mainGUI

openOpenFileDialog :: Window -> MVar GameSG -> MVar Int -> IO ()
openOpenFileDialog parentWindow game navMVar  = do
  dialog <- fileChooserDialogNew
              (Just $ "Load game")
              (Just parentWindow)                     --the parent window
              FileChooserActionOpen                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)
              ,("gtk-open"
               , ResponseAccept)]
                
  fileFilter <- fileFilterNew
  fileFilterAddPattern fileFilter "*.sgf"
  fileChooserAddFilter dialog fileFilter

  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         if (isSuffixOf ".sgf" fileName) then do
                            saveGame <- readFile fileName
                            let maybeGame = parseSGF saveGame
                            if (isRight maybeGame) then do
                                let actGame = fromRight [] maybeGame
                                safeReplaceMVar game actGame
                                safeReplaceMVar navMVar  $ length actGame
                                putStrLn $ "[FILECH] Loaded game " ++ show fileName
                                widgetDestroy parentWindow
                                mainQuit
                            else
                                putStrLn $ "[FILECH] Not able to parse the game: " ++ (fromLeft "Unknown reason" maybeGame)
                         else do 
                            putStrLn "[FILECH] Can only load .sgf files."
    ResponseCancel -> putStrLn "[FILECH] Cancle loading game"
    ResponseDeleteEvent -> putStrLn "[FILECH] Closed file chooser"
  widgetHide dialog

openSaveFileDialog :: Window -> MVar GameSG -> IO ()
openSaveFileDialog parentWindow game = do
  dialog <- fileChooserDialogNew
              (Just $ "Select a file.")
              (Just parentWindow)                           --the parent window
              FileChooserActionSave                         --the kind of dialog we want
              [("gtk-cancel"                                --The buttons to display
               ,ResponseCancel)                             --you can use stock buttons
              ,("gtk-save"
               , ResponseAccept)]

  fileFilter <- fileFilterNew
  fileFilterAddPattern fileFilter "*.sgf"
  fileChooserAddFilter dialog fileFilter
  
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         plain <- readMVar game
                         let saveGame = createSGF plain
                         if (isSuffixOf ".sgf" fileName) then
                             writeGame fileName saveGame
                         else
                             writeGame (fileName ++ ".sgf") saveGame
                         putStrLn $ "[FILECH] Saved the game as " ++ show fileName
                         widgetDestroy parentWindow
                         mainQuit
    ResponseCancel -> putStrLn "[FILECH] Cancle saving game"
    ResponseDeleteEvent -> putStrLn "[FILECH] Closed file chooser"
  widgetHide dialog

writeGame :: String -> String -> IO ()
writeGame filename content = do
    when (length content > 0) $
        writeFile filename content