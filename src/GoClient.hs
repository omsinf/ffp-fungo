module GoClient (connectGo, createClient) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C hiding (drop)
import Control.Concurrent ( forkIO, tryTakeMVar, MVar )
import Control.Monad (unless, forever, void)
import Network.Socket
    ( socketToHandle,
      defaultHints,
      getAddrInfo,
      withSocketsDo,
      connect,
      close,
      AddrInfo(addrSocketType, addrAddress),
      HostName,
      ServiceName,
      Socket,
      SocketType(Stream) )
import Network.Socket.ByteString (recv, sendAll)
import System.IO

import Control.Concurrent.MVar ( tryTakeMVar, MVar )
import Graphics.UI.Gtk ( postGUIAsync )

import ClientUtil
import ModelSG ( Color, GameSG )
import GUIUtil
    ( printToView, resetButtons, MainMenuGUI(text_display) )

-- | creates and returns a socket and opens it on a given address.
createClient :: HostName -> ServiceName -> IO Socket
createClient host port = do
    addr <- resolve
    putStrLn $ "[CLIENT] Running client on " ++ host ++ ":" ++ port
    open addr
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openGoSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

-- | Binds a socket of a given address to a server and transforms it to a handle.
connectGo :: String -> String -> String -> MainMenuGUI -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> IO ()
connectGo host port cname gui mv smv navMVar = withSocketsDo $ do
    soc <- createClient host port
    hdl <- socketToHandle soc ReadWriteMode
    forkIO $ talk hdl cname gui mv smv navMVar
    return ()

-- | The endless loop to receive data and to reacte to received data
communicate :: Handle -> String -> MainMenuGUI -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> IO ()
communicate handle cname gui mv smv navMVar = do
    iseof <- hIsEOF handle
    if iseof then 
      communicate handle cname gui mv smv navMVar
    else do
      resultString <- hGetLine handle
      putStrLn $ "[CLIENT] (<=) " ++ resultString
      react handle resultString cname gui mv smv navMVar
      unless (resultString == "DCA") $ do
            communicate handle cname gui mv smv navMVar

-- | The implementation of the protocol. 
-- The left side of the watch shows the received command. 
-- The right side shows the action to be executed
react :: Handle -> String -> String -> MainMenuGUI -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> IO ()
react handle string cname gui mv smv navMVar = reactTo handle (commandOf string) (removeCommand string) cname gui mv smv navMVar
  where reactTo handle command content cname gui mv smv navMVar
          | command == "DCA" = tryTakeMVar smv >> answer handle "DCA" >> (postGUIAsync $ resetButtons gui)
          | command == "RQN" = processRequestName content cname handle smv
          | command == "RGM" = sendRequestedGame handle mv
          | command == "SGM" = processRequestedGame content mv navMVar gui
          | command == "CHT" = postGUIAsync $ printToView (content ++ "\n") (text_display gui)
          | command == "SMV" = processObtainedMove content mv navMVar gui
          | command == "RMV" = sendRequestedMove handle (read content :: Int) mv
          | command == "SRM" = processRequestedMove content mv navMVar gui
          | otherwise        = return ()
        removeCommand = drop 4
        commandOf = take 3


-- | Initiate the endless communication to the server
talk :: Handle -> String -> MainMenuGUI -> MVar GameSG -> MVar (Handle, Color) -> MVar Int -> IO ()
talk handle cname gui mv smv navMVar = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    forkIO $ communicate handle cname gui mv smv navMVar
    return ()