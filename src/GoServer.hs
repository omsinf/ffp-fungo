module GoServer (runGo) where

import Network.Socket
import System.IO hiding (hPutStrLn)
import Control.Exception ( SomeException(SomeException), handle )
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.ByteString.Char8 as C ( hPutStrLn, pack )
import Data.List ( isPrefixOf )
import Graphics.UI.Gtk ( postGUIAsync )

import ModelSG ( Color )
import GUIUtil ( printToView, MainMenuGUI(text_display) )
import UtilsSG (other)

-- | Run the actual server
runGo :: String -> Color -> MainMenuGUI -> IO ()
runGo port hostColor gui = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    addr <- resolve
    bind sock $ addrAddress addr
    listen sock 2
    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
      (_, _) <- readChan chan
      loop
    mainLoop sock hostColor chan gui 0
  where 
      resolve = do
            let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
            head <$> getAddrInfo (Just hints) Nothing (Just port)

type Msg = (Int, String)

-- | Util function to send a messasge to a specific handle
serverSend :: Handle -> String -> IO ()
serverSend handle string = do
    putStrLn $ "[SERVER] (=>) " ++ string
    hPutStrLn handle $ C.pack string

-- | the the main loop to accept exactly two clients.
mainLoop :: Socket -> Color -> Chan Msg -> MainMenuGUI -> Int -> IO ()
mainLoop sock hostColor chan gui msgNum = do
  if (msgNum == 2) then 
    return()
  else do 
    conn <- accept sock
    forkIO (runConn conn chan msgNum hostColor gui)
    mainLoop sock (other hostColor) chan gui $! msgNum + 1

-- | Run a connection on a socket
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> Color -> MainMenuGUI -> IO ()
runConn (sock, _) chan msgNum hostColor gui = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    serverSend hdl $ "RQN " ++ (show hostColor)
    name <- fmap init (hGetLine hdl)
    broadcast ("CHT " ++ name ++ " joined the game.")
    serverSend hdl $ "CHT Welcome, " ++ name ++ "!"

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ serverSend hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        putStrLn $ "[SERVER] (<=) " ++ line
        if (line == "DCA") then
          serverSend hdl "DCA True"
        else 
          if (isPrefixOf "CHT " line) then 
            broadcast ("CHT " ++ name ++ " ["++ (show hostColor) ++"]: " ++ (drop 4 line)) >> loop
          else
            broadcast line >> loop

    killThread reader                                -- kill after the loop ends
    broadcast ("CHT " ++ name ++ " left the game.")  -- make a final broadcast
    broadcast ("DCA")                                -- close all other handles
    postGUIAsync $ printToView ("Server closed.\n") (text_display gui)
    hClose hdl                                       -- close the handle

{-
Create a server on port 3000 with: 
01: forkIO $ runGo "3000" Black
The color Black means that the first client to connect plays as Black.
Usually this is the player that creates the server (there is a window of half a second where the other client actually can connect first).

The server is meant to deal with exatctly 2 clients at a time.
If two clients are connected it stops listening. In case of an unexpected disconnect the game has to be played again.
It will do nothing but sending received data to the client that did not send the current data initially.
It will however append the name to chat messages.
It also can close the connection to every connected client.

Clients will react automatically to received data.
the following commands can be sent and received:
SRM mid (Send-Requested-Move): if the client asked for the move with id mid then reply to it with the correct move
CHT msg (CHaT): send a chat message
SMV id move (Send-MoVe): send a new move to the client
RQN color (ReQuest-Name): at the beginning the server wants to know whats the name of the client. At the same time it send the client its color
RMV id (Request-MoVe): Ask the other client to send a specific move again. (not implemented yet)
SGM string (Send-GaMe): parse the string to a GameSG and load it
RGM (Request-GaMe): Request the current game state of the other client.

if a client sends DCA then all active connections will be closed.
DCA (DisConnect-All): if the server receives DCA it will end all active connections

To connect a client execute the following commands:
02: handle <- connectGo ...

Message other clients with:
03: hPutStrLn handle "Hello World"

-}
