{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GUIUtil where

import Graphics.UI.Gtk hiding (Socket, Color)
import Graphics.Rendering.Cairo as Cairo
import Control.Lens ( (^.) )
import Control.Monad ( forM_ )
import Control.Concurrent.MVar ( readMVar, MVar ) 

import ModelSG
import LogicSG ( calculatePosition )



----------------------  Data structures holding the gui elements from glade  ----------------------


data MainMenuGUI = MainMenuGUI { window_main             :: Window
                               , grid_main               :: Fixed
                               , box_menu                :: Box
                               , scrolled_window         :: ScrolledWindow
                               , text_display            :: TextView
                               , main_label_name         :: Label
                               , button_new_local_game   :: Button
                               , button_load_game        :: Button
                               , button_server           :: Button
                               , button_connect          :: Button
                               , button_exit             :: Button
                               , button_pass             :: Button
                               , box_moves               :: Box
                               , button_forward_move     :: Button
                               , button_back_move        :: Button
                               , button_completeforward  :: Button
                               , button_completeback     :: Button
                               , event_box               :: EventBox
                               , drawing_area            :: DrawingArea
                               , black_count_textview    :: TextView
                               , white_count_textview    :: TextView
                               , chat_entry              :: Entry
                               , chat_send               :: Button           }


data LoadMenuGUI = LoadMenuGUI { window_load             :: Window
                               , outerbox_load           :: Box
                               , innerbox_load           :: Box
                               , label_load              :: Label
                               , button_abort_load       :: Button
                               , button_ok_load          :: Button
                               , entry_load              :: Entry             }


data ConnectMenuGUI = ConnectMenuGUI { window_connect          :: Window
                                     , outerbox_connect        :: Box
                                     , main_label_connect      :: Label
                                     , ip_entry_connect        :: Entry
                                     , name_label_connect      :: Label
                                     , name_entry_connect      :: Entry
                                     , innerbox_connect        :: Box
                                     , button_ok_connect       :: Button
                                     , button_abort_connect    :: Button        }

data ServerMenuGUI = ServerMenuGUI   { window_server            :: Window
                                     , outerbox_server          :: Box
                                     , main_label_server        :: Label
                                     , ip_entry_server          :: Entry
                                     , name_label_server        :: Label
                                     , name_entry_server        :: Entry
                                     , startcolor_label_server  :: Label
                                     , radiobuttonbox_server    :: Box
                                     , colorblack_server        :: RadioButton
                                     , colorwhite_server        :: RadioButton
                                     , innerbox_server          :: Box
                                     , button_ok_server         :: Button
                                     , button_abort_server      :: Button        }



data AbortMenuGUI = AbortMenuGUI { window_abortgame      :: Window
                                 , outerbox_abortgame    :: Box
                                 , innerbox_abortgame    :: Box
                                 , button_no_abortgame   :: Button
                                 , button_yes_abortgame  :: Button
                                 , label_abortgame       :: Label            }



data AbortServerMenuGUI = AbortServerMenuGUI { window_abortserver      :: Window
                                             , outerbox_abortserver    :: Box
                                             , innerbox_abortserver    :: Box
                                             , button_no_abortserver   :: Button
                                             , button_yes_abortserver  :: Button
                                             , label_abortserver       :: Label            }



data AbortConMenuGUI = AbortConMenuGUI { window_abortcon      :: Window
                                       , outerbox_abortcon    :: Box
                                       , innerbox_abortcon    :: Box
                                       , button_no_abortcon   :: Button
                                       , button_yes_abortcon  :: Button
                                       , label_abortcon       :: Label            }




----------------------  Drawing of the game here ----------------------


-- | Draws the grid as well as the played pieces (stones).
drawEventHandler :: MVar GameSG -> MVar Int -> Render ()
drawEventHandler gameMVar navMVar =
  do
    view <- liftIO $ readMVar navMVar
    game <- liftIO $ readMVar gameMVar
    let pos = calculatePosition $ take view game
    -- Background
    Cairo.rectangle 10 10 580 580
    setSourceRGB 1 0.894 0.7098
    fill
    -- Grid
    setSourceRGB 0 0 0
    forM_ [30,60..570] drawHorizontalLine
    forM_ [30,60..570] drawVerticalLine
    forM_ (cartProd [4,10,16] [4,10,16]) (drawBlackCircleAt 4)
    -- Stones
    markLastMove $ take view game
    drawCircles pos
  where
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]


-- | Marks the last move, if at least one was played and it was no pass.
markLastMove :: GameSG -> Render ()
markLastMove moves
  | length moves == 0 = return ()
  | otherwise = mark $ last moves
  where
    mark :: MoveSG -> Render ()
    mark (MoveSG _ Pass) = return ()
    mark (MoveSG _ (Play (x,y))) = drawGoldenCircle (x,y)
    


-- | Draws all game pieces that are currently visible.
drawCircles :: Either String PositionSG -> Render ()
drawCircles (Right p) = do
    forM_ (_blackStones p) (drawBlackCircleAt 13)
    forM_ (_whiteStones p) drawWhiteCircleAt
drawCircles _ = return ()


-- | Draws a horizontal line with length l from x = 0 and y as height.
drawHorizontalLine :: Double -> Render ()
drawHorizontalLine y = do
  moveTo 30 y
  relLineTo 540 0
drawVerticalLine :: Double -> Render ()
drawVerticalLine x = do
  moveTo x 30
  relLineTo 0 540
  stroke

-- | Draws a White Circle at the entered coordinates (x,y) with radius 13 (default).
drawWhiteCircleAt :: ModelSG.Point -> Render ()
drawWhiteCircleAt (x,y) = do
  setSourceRGB 0.0 0.0 0.0
  Cairo.arc (fromIntegral x*30) (fromIntegral y*30) 13 0 (2 * pi)
  fill
  setSourceRGB 1.0 1.0 1.0
  Cairo.arc (fromIntegral (x*30)) (fromIntegral (y*30)) 12 0 (2 * pi)
  fill


-- | Draws a Black Circle at the entered coordinates (x,y) with radius r (default for game pieces is 13).
-- (We use this function with radius because we need it for more than the game pieces.)
drawBlackCircleAt :: Double -> ModelSG.Point -> Render ()
drawBlackCircleAt r (x,y) = do
  setSourceRGB 0.0 0.0 0.0
  Cairo.arc (fromIntegral (x*30)) (fromIntegral (y*30)) r 0 (2 * pi)
  fill
  

-- | Draws a golden circle at the given point, slightly larger than a stone.
drawGoldenCircle :: ModelSG.Point -> Render ()
drawGoldenCircle (x,y) =
  do
    setSourceRGB 255.0 215.0 0.0
    Cairo.arc (fromIntegral x*30) (fromIntegral y*30) 16 0 (2 * pi)
    fill


----------------------  Util functions for Textstuff  ----------------------


-- | Appends a String in the entered TextView AND scrolls it down to the end of TextView.
-- (Call this once at the start of the GUI to enable the scrolling from the beginning.)
printToView :: TextViewClass t => [Char] -> t -> IO ()
printToView s t = 
  do
    buffer <- textViewGetBuffer t
    iter   <- textBufferGetEndIter buffer
    textBufferInsert buffer iter s
    buffer <- textViewGetBuffer t
    iter   <- textBufferGetEndIter buffer
    mark <- textBufferCreateMark buffer Nothing iter True
    textViewScrollMarkOnscreen t mark


-- | Updates the 2 TextViews associated with the game stats:
-- Number of moves played, player to play next, and each player's captures.
-- (Btw we use a TextView as it doesn't fuck up the background as a label does for unknown reasons.)
updateGameStats :: Either String PositionSG -> MainMenuGUI -> IO ()
updateGameStats (Left _) _ = return ()
updateGameStats (Right position) gui =
  let bc = show (position^.blackCaptures)
      wc = show (position^.whiteCaptures)
      ms = show (position^.movesPlayed)
      nx = show (position^.nextToMove)
  in  updateTextView ("Move #" ++ ms ++ ". Next: " ++ nx) (black_count_textview gui) >>
      updateTextView ("Captures: Black: " ++ bc ++ ", White: " ++ wc) (white_count_textview gui)
  where
    updateTextView :: String -> TextView -> IO ()
    updateTextView s t =
      do
        textBuffer <- textBufferNew Nothing
        textBufferSetText textBuffer s
        textViewSetBuffer t textBuffer


-- | Resets (empties) the entered TextView.
resetView :: TextViewClass t => t -> IO ()
resetView t = do
  textBuffer <- textBufferNew Nothing
  iter   <- textBufferGetEndIter textBuffer
  textViewSetBuffer t textBuffer


-- | Resets the description of the buttons.
resetButtons :: MainMenuGUI -> IO ()
resetButtons gui = do
          buttonSetLabel (button_server gui) ("Start New Server" :: [Char])
          buttonSetLabel (button_connect gui) ("Connect to Server" :: [Char])


----------------------  Loading of Glade files here  ----------------------

-- | Loads Main Menu from Glade files.
loadMainMenu = do               -- runtime error on console if it fails here!
  builder <- builderNew
  builderAddFromFile builder "app/fungo_main.glade"
  window_main            <- builderGetObject builder castToWindow ("window_main" :: [Char])
  grid_main              <- builderGetObject builder castToFixed ("grid_main" :: [Char])
  box_menu               <- builderGetObject builder castToBox ("box_menu" :: [Char])
  scrolled_window        <- builderGetObject builder castToScrolledWindow ("scrolled_window" ::[Char])
  text_display           <- builderGetObject builder castToTextView ("text_display" :: [Char])
  main_label_name        <- builderGetObject builder castToLabel ("label_name" :: [Char])
  button_new_local_game  <- builderGetObject builder castToButton ("button_new_local_game" :: [Char])
  button_load_game       <- builderGetObject builder castToButton ("button_load_game" :: [Char])
  button_server          <- builderGetObject builder castToButton ("button_server" :: [Char])
  button_connect         <- builderGetObject builder castToButton ("button_connect" :: [Char])
  button_exit            <- builderGetObject builder castToButton ("button_exit" :: [Char])
  button_pass            <- builderGetObject builder castToButton ("button_pass" :: [Char])
  box_moves              <- builderGetObject builder castToBox ("box_moves" :: [Char])
  button_forward_move    <- builderGetObject builder castToButton ("button_forward_move" :: [Char])
  button_back_move       <- builderGetObject builder castToButton ("button_back_move" :: [Char])
  button_completeforward <- builderGetObject builder castToButton ("button_completeforward" :: [Char])
  button_completeback    <- builderGetObject builder castToButton ("button_completeback" :: [Char])
  event_box              <- builderGetObject builder castToEventBox ("event_box" :: [Char])
  drawing_area           <- builderGetObject builder castToDrawingArea ("drawing_area" :: [Char])
  black_count_textview   <- builderGetObject builder castToTextView ("black_count_textview" :: [Char])
  white_count_textview   <- builderGetObject builder castToTextView ("white_count_textview" :: [Char])
  chat_entry             <- builderGetObject builder castToEntry ("chat_entry" :: [Char])
  chat_send              <- builderGetObject builder castToButton ("chat_send" :: [Char])
  return $ MainMenuGUI  window_main
                        grid_main
                        box_menu
                        scrolled_window
                        text_display
                        main_label_name
                        button_new_local_game
                        button_load_game
                        button_server
                        button_connect
                        button_exit
                        button_pass
                        box_moves
                        button_forward_move
                        button_back_move
                        button_completeforward
                        button_completeback
                        event_box
                        drawing_area
                        black_count_textview
                        white_count_textview
                        chat_entry
                        chat_send


-- | Loads the load menu with 1 entry.
loadLoadMenu = do               -- runtime error on console if it fails here!
  builder <- builderNew
  builderAddFromFile builder "app/fungo_main.glade"
  window_load            <- builderGetObject builder castToWindow ("window_load" :: [Char])
  outerbox_load          <- builderGetObject builder castToBox ("outerbox_load" :: [Char])
  innerbox_load          <- builderGetObject builder castToBox ("innerbox_load" :: [Char])
  label_load             <- builderGetObject builder castToLabel ("label_load" :: [Char])
  button_abort_load      <- builderGetObject builder castToButton ("button_abort_load" :: [Char])
  button_ok_load         <- builderGetObject builder castToButton ("button_ok_load" :: [Char])
  entry_load             <- builderGetObject builder castToEntry ("entry_load" :: [Char])
  return $ LoadMenuGUI  window_load
                        outerbox_load
                        innerbox_load
                        label_load
                        button_abort_load
                        button_ok_load
                        entry_load


-- | Loads the connect menu with 2 entries.
loadConnectMenu = do             -- runtime error on console if it fails here!
  builder <- builderNew
  builderAddFromFile builder "app/fungo_main.glade"
  window_connect            <- builderGetObject builder castToWindow ("window_connect" :: [Char])
  outerbox_connect          <- builderGetObject builder castToBox ("outerbox_connect" :: [Char])
  innerbox_connect          <- builderGetObject builder castToBox ("innerbox_connect" :: [Char])
  main_label_connect        <- builderGetObject builder castToLabel ("main_label_connect" :: [Char])
  name_label_connect        <- builderGetObject builder castToLabel ("name_label_connect" :: [Char])
  button_abort_connect      <- builderGetObject builder castToButton ("button_abort_connect" :: [Char])
  button_ok_connect         <- builderGetObject builder castToButton ("button_ok_connect" :: [Char])
  ip_entry_connect          <- builderGetObject builder castToEntry ("ip_entry_connect" :: [Char])
  name_entry_connect        <- builderGetObject builder castToEntry ("name_entry_connect" :: [Char])
  return $ ConnectMenuGUI window_connect
                          outerbox_connect
                          main_label_connect
                          ip_entry_connect
                          name_label_connect
                          name_entry_connect
                          innerbox_connect
                          button_ok_connect
                          button_abort_connect


-- | Loads the server menu with 2 entrys. (It is seperate from the connect menu
-- as gtk only hides stuff, but bindings would remain once initiated).
loadServerMenu = do             -- runtime error on console if it fails here!
  builder <- builderNew
  builderAddFromFile builder "app/fungo_main.glade"
  window_server            <- builderGetObject builder castToWindow ("window_server" :: [Char])
  outerbox_server          <- builderGetObject builder castToBox ("outerbox_server" :: [Char])
  innerbox_server          <- builderGetObject builder castToBox ("innerbox_server" :: [Char])
  main_label_server        <- builderGetObject builder castToLabel ("main_label_server" :: [Char])
  name_label_server        <- builderGetObject builder castToLabel ("name_label_server" :: [Char])
  button_abort_server      <- builderGetObject builder castToButton ("button_abort_server" :: [Char])
  button_ok_server         <- builderGetObject builder castToButton ("button_ok_server" :: [Char])
  ip_entry_server          <- builderGetObject builder castToEntry ("ip_entry_server" :: [Char])
  name_entry_server        <- builderGetObject builder castToEntry ("name_entry_server" :: [Char])
  startcolor_label_server  <- builderGetObject builder castToLabel ("startcolor_label_server" :: [Char])
  radiobuttonbox_server    <- builderGetObject builder castToBox ("radiobuttonbox_server" :: [Char])
  colorblack_server        <- builderGetObject builder castToRadioButton ("colorblack_server" :: [Char])
  colorwhite_server        <- builderGetObject builder castToRadioButton ("colorwhite_server" :: [Char])
  return $ ServerMenuGUI window_server
                          outerbox_server
                          main_label_server
                          ip_entry_server
                          name_label_server
                          name_entry_server
                          startcolor_label_server
                          radiobuttonbox_server
                          colorblack_server
                          colorwhite_server
                          innerbox_server
                          button_ok_server
                          button_abort_server


-- | Loads the abort menu for new games
loadAbortMenu :: IO AbortMenuGUI
loadAbortMenu = do               -- runtime error on console if it fails here!
  builder <- builderNew
  builderAddFromFile builder "app/fungo_main.glade"
  window_abortgame       <- builderGetObject builder castToWindow ("window_abortgame" :: [Char])
  outerbox_abortgame     <- builderGetObject builder castToBox ("outerbox_abortgame" :: [Char])
  innerbox_abortgame     <- builderGetObject builder castToBox ("innerbox_abortgame" :: [Char])
  button_no_abortgame    <- builderGetObject builder castToButton ("button_no_abortgame" :: [Char])
  button_yes_abortgame   <- builderGetObject builder castToButton ("button_yes_abortgame" :: [Char])
  label_abortgame        <- builderGetObject builder castToLabel ("label_abortgame" :: [Char])
  return $ AbortMenuGUI  window_abortgame
                         outerbox_abortgame
                         innerbox_abortgame
                         button_no_abortgame
                         button_yes_abortgame
                         label_abortgame


-- | Loads the abort menu for server closing
loadAbortServerMenu :: IO AbortServerMenuGUI
loadAbortServerMenu = do               -- runtime error on console if it fails here!
  builder <- builderNew
  builderAddFromFile builder "app/fungo_main.glade"
  window_abortserver       <- builderGetObject builder castToWindow ("window_abortserver" :: [Char])
  outerbox_abortserver     <- builderGetObject builder castToBox ("outerbox_abortserver" :: [Char])
  innerbox_abortserver     <- builderGetObject builder castToBox ("innerbox_abortserver" :: [Char])
  button_no_abortserver    <- builderGetObject builder castToButton ("button_no_abortserver" :: [Char])
  button_yes_abortserver   <- builderGetObject builder castToButton ("button_yes_abortserver" :: [Char])
  label_abortserver        <- builderGetObject builder castToLabel ("label_abortserver" :: [Char])
  return $ AbortServerMenuGUI  window_abortserver
                               outerbox_abortserver
                               innerbox_abortserver
                               button_no_abortserver
                               button_yes_abortserver
                               label_abortserver


-- | Loads the abort menu for connection closing
loadAbortConMenu :: IO AbortConMenuGUI
loadAbortConMenu = do               -- runtime error on console if it fails here!
  builder <- builderNew
  builderAddFromFile builder "app/fungo_main.glade"
  window_abortcon       <- builderGetObject builder castToWindow ("window_abortconnect" :: [Char])
  outerbox_abortcon     <- builderGetObject builder castToBox ("outerbox_abortconnect" :: [Char])
  innerbox_abortcon     <- builderGetObject builder castToBox ("innerbox_abortconnect" :: [Char])
  button_no_abortcon    <- builderGetObject builder castToButton ("button_no_abortconnect" :: [Char])
  button_yes_abortcon   <- builderGetObject builder castToButton ("button_yes_abortconnect" :: [Char])
  label_abortcon        <- builderGetObject builder castToLabel ("label_abortconnect" :: [Char])
  return $ AbortConMenuGUI  window_abortcon
                            outerbox_abortcon
                            innerbox_abortcon
                            button_no_abortcon
                            button_yes_abortcon
                            label_abortcon

