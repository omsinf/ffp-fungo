{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GUI (mainGoGUI) where

import Control.Concurrent
import Control.Concurrent.MVar  
-- This is a workaround to be used for global variables
-- as we can't recursively pass down i.e. a changed game state.
-- It would be possible to wrap this in a state monad or to execute the game in a thread
-- and simply insert the commands and extract the game state to draw it, but that would be
-- the same with extra steps.
-- We know that it would be better in an advanced program because of the state monad/
-- monad transformer though.
import Control.Exception
import Control.Monad ( when, unless ) 
import Data.List.Utils (replace)
import Data.Maybe ( fromJust, isJust )
import Graphics.UI.Gtk hiding (Socket, Color)
import Graphics.Rendering.Cairo ( liftIO )
import Network.Socket.ByteString (recv, sendAll)
import System.IO ( Handle )
import Text.Read ( readMaybe )

import GUIUtil

import ModelSG
import UtilsSG ( emptyBoard, other, showLegible )
import LogicSG ( calculatePosition, play )
import GoServer ( runGo )
import GoClient ( connectGo )
import ClientUtil
import FileChooser ( openChooser )



type IsNetworkEmpty = Bool
type IsForwardBackwardEmpty = Bool
type MVarErrorMsg = String

mainGoGUI :: IO ()
mainGoGUI = do
  initGUI                                  -- inits GTK+
  gui            <- loadMainMenu
  loadgui        <- loadLoadMenu
  abortgui       <- loadAbortMenu
  abortServergui <- loadAbortServerMenu
  abortCongui    <- loadAbortConMenu
  connectgui     <- loadConnectMenu
  servergui      <- loadServerMenu
  gameMVar       <- newMVar ([] :: GameSG)             -- This MVar holds the game. Its NEVER empty! The empty game is a empty list of turns
  serverMVar     <- newEmptyMVar                       -- This MVar holds a tupel of (client-socket, color) for multiplayer and is empty of no mp game is in progress!
  navMVar        <- newMVar (0 :: Int)                 -- This MVar holds the state for going backwards and forwards in the game to view different moves/states. Its empty if not used
  resetView (text_display gui)                         -- This is to get the mark in the textview
  widgetShowAll (window_main gui)
  on (drawing_area gui)                draw                $ drawEventHandler gameMVar navMVar
  on (event_box gui)                   buttonPressEvent    $ tryEvent $ do                   
    coords <- eventCoordinates                                            
    liftIO $ callbackPlay coords gameMVar gui serverMVar navMVar                                     
  on (button_new_local_game gui)       buttonActivated     $ callbackNewGame gui abortgui gameMVar navMVar serverMVar
  on (button_load_game gui)            buttonActivated     $ callbackLoadGame gui gameMVar navMVar serverMVar 
  on (button_server gui)               buttonActivated     $ callbackCreateServer gui servergui abortServergui gameMVar serverMVar navMVar
  on (button_connect gui)              buttonActivated     $ callbackConnectClient gui connectgui abortCongui gameMVar serverMVar navMVar
  on (button_exit gui)                 buttonActivated     $ saveQuit serverMVar gui                             
  on (button_pass gui)                 buttonActivated     $ callbackPass gui gameMVar serverMVar navMVar 
  on (button_completeback gui)         buttonActivated     $ callbackToBegin gui navMVar
  on (button_back_move gui)            buttonActivated     $ callbackBackward gui gameMVar navMVar
  on (button_forward_move gui)         buttonActivated     $ callbackForward gui gameMVar navMVar
  on (button_completeforward gui)      buttonActivated     $ callbackToEnd gui  gameMVar navMVar
  on (button_abort_server servergui)   buttonActivated     $ widgetHide (window_server servergui)
  on (button_ok_server servergui)      buttonActivated     $ tryCreateServer gui servergui abortgui gameMVar serverMVar navMVar
  on (button_abort_connect connectgui) buttonActivated     $ widgetHide (window_connect connectgui)
  on (button_ok_connect connectgui)    buttonActivated     $ getTextTryConnect gui connectgui gameMVar serverMVar navMVar
  on (chat_send gui)                   buttonActivated     $ callbackSendChat gui serverMVar
  on (button_no_abortgame abortgui)    buttonActivated     $ widgetHide (window_abortgame abortgui)
  on (button_yes_abortgame abortgui)   buttonActivated     $ resetGame gameMVar navMVar gui abortgui
  on (button_no_abortserver abortServergui) buttonActivated  $ widgetHide (window_abortserver abortServergui)
  on (button_yes_abortserver abortServergui) buttonActivated $ closeConnection serverMVar gui >> widgetHide (window_abortserver abortServergui)
  on (button_no_abortcon abortCongui) buttonActivated     $ widgetHide (window_abortcon abortCongui)
  on (button_yes_abortcon abortCongui) buttonActivated    $ closeConnection serverMVar gui >> widgetHide (window_abortcon abortCongui)
  on (window_main gui)                 objectDestroy       $ saveQuit serverMVar gui       
  mainGUI                                                                            -- runs GTK+ main loop as long as GUI is not closed

callbackSendChat :: MainMenuGUI -> MVar (Handle, Color) -> IO ()
callbackSendChat gui serverMVar = 
  do
    chatmsg <- entryGetText (chat_entry gui) :: (IO [Char])
    networkEmpty <- isEmptyMVar serverMVar
    let replaceNewline = (\msg -> replace "\n" " " msg)
    if (networkEmpty) 
      then
        unless (trimName (replaceNewline chatmsg) == "") $ 
          printToView ("You: " ++ chatmsg ++ "\n") (text_display gui)
      else do
        (handle, color) <- readMVar serverMVar
        unless (trimName (replaceNewline chatmsg) == "") $ 
          chat handle color gui (replaceNewline chatmsg)
    entrySetText (chat_entry gui) ("" :: [Char])


-- | Checks if you want to save before quitting
saveQuit :: MVar (Handle, Color) -> MainMenuGUI -> IO ()
saveQuit serverMVar gui = do
    isEmpty <- isEmptyMVar serverMVar
    if (isEmpty) then 
      mainQuit
    else do
      closeConnection serverMVar gui
      mainQuit


-- | Goes to the end of the current game (which is a precondition for any other action).
callbackToEnd :: MainMenuGUI -> MVar GameSG -> MVar Int -> IO ()
callbackToEnd gui gameMVar navMVar =
  do
    game <- readMVar gameMVar
    safeReplaceMVar navMVar (length game)
    updateGameStats (calculatePosition game) gui
    widgetQueueDraw (drawing_area gui)


-- | Goes one move forward in the current game.
callbackForward :: MainMenuGUI -> MVar GameSG -> MVar Int -> IO ()
callbackForward gui gameMVar navMVar =
  do
    game <- readMVar gameMVar
    view <- readMVar navMVar
    let view' = if view < (length game) then view + 1 else (length game)
    safeReplaceMVar navMVar view'
    updateGameStats (calculatePosition $ take view' game) gui
    widgetQueueDraw (drawing_area gui)


-- | Goes one move backward in the current game.
callbackBackward :: MainMenuGUI -> MVar GameSG -> MVar Int -> IO ()
callbackBackward  gui gameMVar navMVar =
  do
    game <- readMVar gameMVar
    view <- readMVar navMVar
    let view' = if view > 0 then view - 1 else 0
    safeReplaceMVar navMVar view'
    updateGameStats (calculatePosition $ take view' game) gui
    widgetQueueDraw (drawing_area gui)


-- | Goes to the begin of the current game.
callbackToBegin :: MainMenuGUI -> MVar Int -> IO ()
callbackToBegin gui navMVar = safeReplaceMVar navMVar 0
                              >> widgetQueueDraw (drawing_area gui)
                              >> updateGameStats (Right emptyBoard) gui


-- | Play button handling the playing of a piece at the coordinates entered
callbackPlay :: (Double, Double) -> MVar GameSG -> MainMenuGUI -> MVar (Handle, Color) -> MVar Int -> IO ()
callbackPlay (x,y) gameMVar gui serverMVar navMVar = do
  size <- widgetGetSizeRequest (event_box gui) -- This returns IO (x,y) of the requested size of the play area (600/600)atm fixed
  let col = (getPlayPosition x (getx size))
  let row = (getPlayPosition y (gety size))
  when (validPosition col row) $ do
      fbbool <- lastMoveOnDisplay gameMVar navMVar
      isLocal <- isEmptyMVar serverMVar
      game <- readMVar gameMVar
      let curColor = getCurrentPlayer game
      playerColor <- getPlayerColor isLocal serverMVar curColor
      let move = MoveSG playerColor (Play (col,row))
      tryPlayMove fbbool isLocal curColor playerColor move gui gameMVar navMVar serverMVar
  where getx (x,_) = fromIntegral x
        gety (_,y) = fromIntegral y
        validPosition col row = (col /= (-1)) && (row /= (-1))
        getPlayerColor True _ c  = return c
        getPlayerColor False serverMVar c = do
                                 (_,color) <- readMVar serverMVar
                                 return color

-- Get the played row/column. c = coordinate; b = width/height of play area. We use a 19x19 area and start with 0
-- If the entered position is bullshit we return -1 (Outside of b is not possible by default!)
-- But we dont want to be able to click really close to the border because thats just problems when playing
getPlayPosition :: Double -> Double -> Int
getPlayPosition c b 
  | c < ((b/20)/2) = -1
  | c > (b-((b/20)/2)) = -1
  | otherwise = (round(c/(b/20)))


-- | Returns the player to move next in a given game.
getCurrentPlayer :: GameSG -> Color
getCurrentPlayer [] = Black   -- Black is the beginning player.
getCurrentPlayer x  = other (_color (last x))


callbackPass :: MainMenuGUI -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> IO ()
callbackPass gui gameMVar serverMVar navMVar = do
  fbbool <- lastMoveOnDisplay gameMVar navMVar
  isLocal <- isEmptyMVar serverMVar
  game <- readMVar gameMVar
  let curColor = getCurrentPlayer game
  playerColor <- getPlayerColor isLocal serverMVar curColor
  let move = MoveSG playerColor Pass
  tryPlayMove fbbool isLocal curColor playerColor move gui gameMVar navMVar serverMVar
  where getPlayerColor True _ c  = return c
        getPlayerColor False serverMVar c = do
                                 (_,color) <- readMVar serverMVar
                                 return color


-- | Plays a move, if possible, or explains why playing is not possible at the moment.
tryPlayMove :: Bool -> Bool -> Color -> Color -> MoveSG -> MainMenuGUI -> MVar GameSG -> MVar Int -> MVar (Handle, Color) -> IO ()
tryPlayMove fb b curColor playerColor newMove gui gameMVar navMVar serverMVar
  | not fb = printToView ("You are currently viewing an old game state.\nTo do something please return to the current state.\n") (text_display gui)
  | not b && (curColor /= playerColor) = printToView ("Wait for your opponent to play.\n") (text_display gui)
  | otherwise = do
                  game <- readMVar gameMVar
                  case (play newMove game) of
                    Left string   -> printToView (string ++ "\n") (text_display gui)
                    Right newGame -> do
                                       swapMVar gameMVar newGame
                                       swapMVar navMVar $ length newGame
                                       printToView ((showLegible newMove) ++ "\n") (text_display gui)
                                       widgetQueueDraw (drawing_area gui)
                                       updateGameStats (calculatePosition newGame) gui
                                       ifMPSend serverMVar newGame
  where ifMPSend serverMVar game = do
                              isempty <- isEmptyMVar serverMVar
                              when (not isempty) $ do
                                  (socket, col) <- readMVar serverMVar
                                  sendMove socket $ last game
 

-- | Loads a game from a SGF file.
callbackLoadGame :: MainMenuGUI -> MVar GameSG -> MVar Int -> MVar (Handle, Color) -> IO ()
callbackLoadGame gui gameMVar navMVar serverMVar = 
  do
    isCurrentState <- lastMoveOnDisplay gameMVar navMVar
    isClient <- isEmptyMVar serverMVar
    let errorMsg = isCallbackAllowed isClient isCurrentState
    if (errorMsg /= "OK.") 
      then 
        printToView errorMsg (text_display gui)
      else do
        openChooser gameMVar navMVar
        widgetQueueDraw (drawing_area gui)
        game <- readMVar gameMVar
        updateGameStats (calculatePosition game) gui


-- | Starts a new game.
callbackNewGame :: MainMenuGUI -> AbortMenuGUI -> MVar GameSG -> MVar Int -> MVar (Handle, Color) -> IO ()
callbackNewGame gui abortgui gameMVar navMVar serverMVar = 
  do
    isCurrentState <- lastMoveOnDisplay gameMVar navMVar
    isClient       <- isEmptyMVar serverMVar
    let errorMsg = isCallbackAllowed isClient isCurrentState
    if (errorMsg /= "OK.")
      then  
        printToView errorMsg (text_display gui)
      else do 
        labelSetText (label_abortgame abortgui) (" Do you want to cancel your current game and start a new game? " :: [Char])
        windowPresent (window_abortgame abortgui)

-- | Resets the game. Displays empty board.
resetGame :: MVar GameSG -> MVar Int -> MainMenuGUI -> AbortMenuGUI -> IO ()
resetGame gameMVar navMVar gui abortgui = do
              swapMVar navMVar 0
              swapMVar gameMVar ([] :: GameSG)
              widgetHide (window_abortgame abortgui)
              resetView (text_display gui)
              printToView ("New local game started\n") (text_display gui)
              updateGameStats (Right emptyBoard) gui


callbackConnectClient :: MainMenuGUI -> ConnectMenuGUI -> AbortConMenuGUI -> MVar GameSG -> MVar (Handle, b) -> MVar Int -> IO ()
callbackConnectClient gui cgui abortgui gameMVar serverMVar navMVar = 
  do
    bool <- lastMoveOnDisplay gameMVar navMVar
    if not bool then printToView ("You are currently viewing an old game state.\nTo do something please return to the current state.\n") (text_display gui)
      else do
        connectionStatus <- isEmptyMVar serverMVar
        if connectionStatus then do
          labelSetText (main_label_connect cgui) ("    Enter to IP-address and port of the server you want to connect to:   \n    Example: 127.0.0.1:3000       \n    Note: Connecting to a server will end your current game!   " :: [Char])
          labelSetText (name_label_connect cgui) ("    Enter your name for the game:     " :: [Char])
          windowPresent (window_connect cgui)
        else do
          windowPresent (window_abortcon abortgui)


getTextTryConnect :: MainMenuGUI -> ConnectMenuGUI -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> IO ()
getTextTryConnect gui cgui mv serverMVar navMVar = do
                   name <- entryGetText (name_entry_connect cgui) :: (IO [Char])  -- TODO: Check for empty
                   ip <- entryGetText (ip_entry_connect cgui) :: (IO [Char])
                   widgetHide (window_connect cgui)
                   safeReplaceMVar mv ([] :: GameSG)
                   tryConnect gui ip mv serverMVar navMVar name


-- Connect to server IP string and start a new, empty game in MVar mv (game) as well as send it to other player once connected
tryConnect :: MainMenuGUI -> String -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> [Char] -> IO ()
tryConnect gui string mv serverMVar navMVar name =
    do
      case splitIP string of
        Nothing -> printToView "Invalid ip and port\n" (text_display gui) 
        Just (ip, port) -> do
                            let fName = if ((trimName name) == "") then "Unknown" else name
                            catch (connectGo ip port fName gui mv serverMVar navMVar
                              >> buttonSetLabel (button_connect gui) ("Close Connection" :: [Char])
                              >> return ()) excepHandler
                            where
                              excepHandler :: SomeException -> IO ()
                              excepHandler e = do
                                putStrLn $ "[FGOGUI] The host is not reachable: " ++ show e
                                printToView "The host is not reachabel.\n" (text_display gui)


-- Close the connection to all connected entities.
closeConnection :: MVar (Handle, b) -> MainMenuGUI -> IO ()
closeConnection serverMVar gui = do
    maybeTuple <- tryTakeMVar serverMVar
    when (isJust maybeTuple) $ do
        let (handle, _) = fromJust maybeTuple 
        answer handle "DCA"
        buttonSetLabel (button_connect gui) ("Connect to Server" :: [Char])


-- | Create a server and connect the host client immediatley.
callbackCreateServer :: MainMenuGUI -> ServerMenuGUI -> AbortServerMenuGUI -> MVar GameSG -> MVar (Handle, b) -> MVar Int -> IO ()
callbackCreateServer gui sgui abortgui gameMVar serverMVar navMVar = 
  do
    isCurrGame <- lastMoveOnDisplay gameMVar navMVar
    if not isCurrGame then printToView ("You are currently viewing an old game state.\nTo do something please return to the current state.\n") (text_display gui)
      else do 
        serverStatus <- isEmptyMVar serverMVar
        if serverStatus then do
          labelSetText (main_label_server sgui) ("   Enter the port for the server you want to create:   \n   Note: This will lift your current game to the newly created server!   " :: [Char])
          labelSetText (name_label_server sgui) ("    Enter your name for the game:     " :: [Char])
          windowPresent (window_server sgui)
          else do
            windowPresent (window_abortserver abortgui)


-- | Create a server if possible
tryCreateServer :: MainMenuGUI -> ServerMenuGUI -> p1 -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> IO ()
tryCreateServer gui sgui abortgui mv serverMVar navMVar = do
                   whitebool <- toggleButtonGetActive (colorwhite_server sgui)  -- This is TRUE if white is selected
                   name <- entryGetText (name_entry_server sgui) :: (IO [Char])  -- TODO: Check for valid name
                   ip <- entryGetText (ip_entry_server sgui)
                   widgetHide (window_server sgui)
                   if (checkPort ((readMaybe ip) :: Maybe Int))
                     then do 
                            putStrLn "[FGOGUI] Starting server..."
                            forkIO $ runGo ip (if whitebool then White else Black) gui
                            threadDelay 500000
                            putStrLn "[FGOGUI] Server is now running..."
                            tryConnect gui ("127.0.0.1:" ++ ip) mv serverMVar navMVar name
                            putStrLn "[FGOGUI] Client successfully connected to the server."
                            buttonSetLabel (button_server gui) ("Close Server" :: [Char])
                     else do
                            putStrLn "[FGOGUI] Cannot start the server. The given port is not in the valid range."


-- | Check if the callback to a function is allowed. 
-- The first argument wants to know if the networkMVar is empty, the second if the fbmvar is empty.
isCallbackAllowed :: IsNetworkEmpty -> IsForwardBackwardEmpty -> MVarErrorMsg
isCallbackAllowed True False = "You are currently viewing an old game state.\nTo do something please return to the current state.\n"
isCallbackAllowed False True = "You are currently playing a multiplayer game.\nDisconnect to proceed.\n"
isCallbackAllowed False False = "You are currently viewing an old game state\nand you are playing a multiplayer game.\nReturn to the current state and disconnect form the server to proceed.\n"
isCallbackAllowed True True =  "OK."


-- | Checks if the displayed game state is the "present" one.
lastMoveOnDisplay :: MVar GameSG -> MVar Int -> IO Bool
lastMoveOnDisplay gameMVar navMVar = (==) <$> (length <$> readMVar gameMVar) <*> readMVar navMVar
