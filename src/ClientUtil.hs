module ClientUtil ( openGoSocket, 
                       processObtainedMove,
                       processRequestedMove,
                       processRequestName,
                       processRequestedGame,
                       sendMove,
                       sendRequestedMove, 
                       sendRequestedGame,
                       answer, 
                       splitIP, 
                       getIPv4Adress, 
                       checkPort,
                       chat,
                       trimName,
                       safeReplaceMVar ) 
where

import Data.List.Split ( splitOn )
import Data.Maybe ( fromJust )
import Data.List ( dropWhileEnd )
import Data.Either ( fromLeft, fromRight, isRight )
import Data.Char(isSpace)
import Data.List.Utils (replace)

import Network.Socket
    ( socket,
      AddrInfo(addrFamily, addrSocketType, addrProtocol),
      Socket )
import Network.Info
    ( getNetworkInterfaces, IPv4, NetworkInterface(ipv4) )
import Network.Socket.ByteString (sendAll)

import Text.Read ( readMaybe )
import System.IO ( hPutStrLn, Handle )

import Control.Concurrent.MVar
    ( swapMVar, isEmptyMVar, putMVar, readMVar, MVar )
import Control.Lens ( (^.) )

import Graphics.UI.Gtk ( widgetQueueDraw, postGUIAsync )

import ModelSG
    ( color, Color(..), GameSG, MoveGo(..), MoveSG(MoveSG) )
import GUIUtil
    ( printToView,
      MainMenuGUI(drawing_area, text_display),
      updateGameStats )
import LogicSG ( calculatePosition )
import ParseSG (createSGF, parseSGF)

endingSequence :: String
endingSequence = " "

-- | Wrapper for opening a socket
openGoSocket :: AddrInfo -> IO Socket
openGoSocket = (\addr -> socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))

-- | Check if a string represents a valid move. If it does return 'Just move' otherwise return 'Nothing'
validMove :: String -> Maybe MoveSG
validMove (x:xs) 
        | x == 'B' = helper Black (extractMove xs)
        | x == 'W' = helper White (extractMove xs)
        | otherwise = Nothing
    where 
        helper color (Just Pass) = Just $ MoveSG color Pass
        helper color (Just mgo) = Just $ MoveSG color mgo 
        helper _ _ = Nothing

-- move has to have the form C[x,y] where C is a color and x,y are integers or [] for Pass
extractMove:: String -> Maybe MoveGo
extractMove "[]" = Just Pass
extractMove str = toMove str
    where
        toMove string = prepare $ splitOn "," (replace "]" "" (replace "[" "" string))
        prepare [a,b] = Just $ Play ((read a :: Int), (read b :: Int))
        prepare _ = Nothing

-- | Write a message to a handle
answer :: Handle -> String -> IO ()
answer handle string = hPutStrLn handle $ string ++ endingSequence

-- Splits entered IP string (e.g. 127.0.0.1:3000) into IP and port if possible, nothing else
splitIP :: String -> Maybe (String,String)
splitIP str = processParts $ splitOn ":" str
    where
        processParts [ip, port] = checkAll (processIP ip, processPort port)
        processParts _ = Nothing
        processIP ip = checkIP ip $ splitOn "." ip
        checkIP ip [p1, p2, p3, p4] = 
            if ((checkRange p1) && (checkRange p2) && (checkRange p3) && (checkRange p4)) 
                then Just ip 
                else Nothing
        checkIP _ _ = Nothing
        checkRange number = inRange (readMaybe number :: Maybe Int)
        inRange Nothing = False
        inRange (Just number)
                    | number < 0 = False
                    | number > 255 = False
                    | otherwise = True
        processPort port = 
            if (checkPort (readMaybe port :: Maybe Int))
                then Just port
                else Nothing
        checkAll (Nothing, _) = Nothing
        checkAll (_, Nothing) = Nothing
        checkAll (Just ip, Just port) = Just (ip, port)

-- | Check if a port is within the valid range
checkPort :: (Maybe Int) -> Bool
checkPort (Just port)
            | port <  1024 = False
            | port > 65535 = False
            | otherwise = True
checkPort Nothing = False

-- | Get the IPv4 adress of all network interfaces of the local machine
getIPv4Adress :: IO [IPv4]
getIPv4Adress = fmap (map ipv4) getNetworkInterfaces

-- | Process the obtained move and draw it to pac> MainMenuGUI -> IO ()
processObtainedMove :: [Char] -> MVar [MoveSG] -> MVar Int -> MainMenuGUI -> IO ()
processObtainedMove move mv navMVar gui = do
                   let newMove = fromJust (validMove move)
                   game <- readMVar mv
                   let newGame = game ++ [newMove]
                   safeReplaceMVar mv newGame
                   view <- readMVar navMVar
                   let view' = if view < (length game) then view else view + 1
                   safeReplaceMVar navMVar view'
                   postGUIAsync $ widgetQueueDraw (drawing_area gui)
                   postGUIAsync $ printToView ((show (newMove^.color)) ++ " played. It's your turn.\n") (text_display gui)
                   postGUIAsync $ updateGameStats (calculatePosition newGame) gui

-- | If a requested move is received replace the move in the current list, append or discard it.
processRequestedMove :: String -> MVar GameSG -> MVar Int -> MainMenuGUI -> IO ()
processRequestedMove move mv navMVar gui = do
                    [str_id, move] <- return $ splitOn " " move
                    let id = read str_id :: Int
                    let actMove = fromJust (validMove move)
                    game <- readMVar mv
                    if (length game > id) then do
                        let game' = replaceNth id actMove game
                        safeReplaceMVar mv game'
                    else do
                        if (id == length game) then do
                           let newGame = game ++ [actMove]
                           safeReplaceMVar mv newGame
                           view <- readMVar navMVar
                           let view' = if view < (length game) then view + 1 else (length game)
                           safeReplaceMVar navMVar view'
                           postGUIAsync $ widgetQueueDraw (drawing_area gui)
                           postGUIAsync $ printToView ((show (actMove^.color)) ++ " played. It's your turn.\n") (text_display gui)
                           postGUIAsync $ updateGameStats (calculatePosition newGame) gui
                        else do
                           putStrLn "[CLIENT] The requested move is not in range. Request a move with a smaller id."

-- | If a requested game is received draw it to the GUI
processRequestedGame :: String -> MVar GameSG -> MVar Int -> MainMenuGUI -> IO ()
processRequestedGame game mv navMVar gui  = do
                    let maybeGame = parseSGF game 
                    if (isRight maybeGame) then do
                        let actGame = fromRight ([] :: GameSG) maybeGame
                        safeReplaceMVar mv actGame
                        safeReplaceMVar navMVar $ length actGame
                        postGUIAsync $ widgetQueueDraw (drawing_area gui)
                        postGUIAsync $ printToView "Game loaded...\n" (text_display gui)
                        postGUIAsync $ updateGameStats (calculatePosition actGame) gui
                    else do
                        safeReplaceMVar mv ([] :: GameSG)
                        safeReplaceMVar navMVar 0
                        putStrLn $ "[CLIENT] Not able to parse the game: " ++ (fromLeft "Unknown reason" maybeGame)

-- | Chat with the other person in a multiplayer game
chat :: Handle -> Color -> MainMenuGUI -> String -> IO ()
chat handle color gui msg = do
    answer handle $ "CHT " ++ msg
    postGUIAsync $ printToView ("You [" ++ (show color) ++ "]: " ++ msg ++ "\n") (text_display gui)

-- | Send a move to the other client
sendMove :: Handle -> MoveSG -> IO ()
sendMove handle move = answer handle $ "SMV " ++ (show move)

-- | Send the requested move with a certain id back to the client
sendRequestedMove :: Handle -> Int -> MVar GameSG -> IO ()
sendRequestedMove handle id mv = do
    list <- readMVar mv
    if (id >= length list) then 
        putStrLn "[CLIENT] The requested id is out of range."
    else do
        let move = list!!id
        answer handle $ "SRM " ++ (show id) ++ " " ++ (show move)

-- | Broadcast the name of this client and receive the color played by this client
processRequestName :: String -> String -> Handle -> MVar (Handle, Color) -> IO ()
processRequestName msg cname handle smv = do
        answer handle cname
        answer handle "RGM"
        case msg of
            "Black" -> safeReplaceMVar smv (handle, Black)
            _       -> safeReplaceMVar smv (handle, White)

-- | Encode the current game in the sgf format and send it to the other client
sendRequestedGame :: Handle -> MVar GameSG -> IO ()
sendRequestedGame handle mv = do
    list <-readMVar mv
    answer handle $ "SGM " ++ (createSGF list)

-- | Safely replaces the MVar depending on the current status of it
safeReplaceMVar :: MVar a -> a -> IO ()
safeReplaceMVar mv a = do
  isEmpty <- isEmptyMVar mv
  if isEmpty then do
    putMVar mv a
    return ()
  else do
    swapMVar mv a
    return ()

-- | Remove all leading and trailing whitespaces form a String
trimName :: String -> String
trimName = dropWhileEnd isSpace . dropWhile isSpace

-- | Replace the n-th element of a list.
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs