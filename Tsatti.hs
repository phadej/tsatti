{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Tsatti (tsattiApplication, pinger) where

-- https://github.com/yesodweb/wai/blob/master/wai-websockets/server.lhs
-- http://jaspervdj.be/websockets/example.html

import Network.Wai (Application, Middleware)
import Network.Wai.Application.Static (staticApp, embeddedSettings)
-- import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Network.WebSockets as WS

import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import Data.Monoid ((<>))
import Data.List (find)
import Data.Maybe (isNothing)

import Control.Exception (handle, SomeException, throwTo)

import Control.Monad (forM_, forever)
import Control.Applicative

import Control.Concurrent (threadDelay, ThreadId, myThreadId)
import Control.Concurrent.STM (TVar, STM, atomically, readTVar, readTVarIO, writeTVar)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (ToJSON, FromJSON, Value(..), object, encode, decode, (.:), (.=))
import qualified Data.Aeson.Types as Aeson

import Data.FileEmbed (embedDir)

data Client = Client {
  clientNick :: Text,
  clientConnection :: WS.Connection,
  clientActive :: UTCTime,
  clientThreadId :: ThreadId
}

type ServerState = [Client]

lookupClient :: Text -> ServerState -> Maybe Client
lookupClient nick = find ((==nick) . clientNick)

data DownMessage = PingMessage
                 | TextMessage Text Text
                 | PrivateMessage Text Text
                 | NickChangeMessage Text Text
                 | UserJoinedMessage Text
                 | UserLeftMessage Text
                 | UsersListMessage [Text]
                 | ErrorMessage Text
  deriving (Eq, Show)

instance ToJSON DownMessage where
  toJSON PingMessage                  = object [ "type" .= ("ping"  :: Text) ]
  toJSON (TextMessage sender msg)     = object [ "type" .= ("msg"   :: Text) , "sender" .= sender, "msg" .= msg ]
  toJSON (PrivateMessage sender msg)  = object [ "type" .= ("priv"  :: Text) , "sender" .= sender, "msg" .= msg ]
  toJSON (NickChangeMessage old new)  = object [ "type" .= ("nick"  :: Text) , "old" .= old, "new" .= new ]
  toJSON (UserJoinedMessage nick)     = object [ "type" .= ("join"  :: Text) , "nick" .= nick ]
  toJSON (UserLeftMessage nick)       = object [ "type" .= ("part"  :: Text) , "nick" .= nick ]
  toJSON (UsersListMessage ns)        = object [ "type" .= ("list"  :: Text) , "nicks" .= ns ]
  toJSON (ErrorMessage msg)           = object [ "type" .= ("error" :: Text) , "msg" .= msg ]

data UpMessage = UpTextMessage Text
               | UpPrivateMessage Text Text
               | UpNickMessage Text
               | UpListMessage
               | UpPongMessage
  deriving (Eq, Show)

instance FromJSON UpMessage where
  parseJSON (Object v) = do
    t <- v .: "type" :: Aeson.Parser Text
    case t of
      "msg"  -> UpTextMessage <$> v .: "msg"
      "priv" -> UpPrivateMessage <$> v .: "nick" <*> v .: "msg"
      "nick" -> UpNickMessage <$> v .: "nick"
      "list" -> return UpListMessage
      "pong" -> return UpPongMessage
      _     -> empty
  parseJSON _ = empty

broadcast :: ServerState -> DownMessage -> IO ()
broadcast state message = do
  clk <- getCurrentTime
  putStrLn $ show clk ++ " BROADCAST: " ++ show message
  forM_ state $ \client -> WS.sendTextData (clientConnection client) $ encode message

send :: WS.Connection -> DownMessage -> IO ()
send conn message = do
  clk <- getCurrentTime
  putStrLn $ show clk ++ " SEND: " ++ show message
  WS.sendTextData conn $ encode message

sendUsersList :: WS.Connection -> ServerState -> IO ()
sendUsersList conn s = send conn $ UsersListMessage $ map clientNick s

websocketsApplication :: TVar ServerState -> WS.ServerApp
websocketsApplication state pending = do
  tid <- myThreadId
  clk <- getCurrentTime
  conn <- WS.acceptRequest pending
  (s, nick) <- atomically $ newNickState state clk conn tid
  sendUsersList conn s
  broadcast s $ UserJoinedMessage nick
  talk state $ Client nick conn clk tid

newNickState :: TVar ServerState -> UTCTime -> WS.Connection -> ThreadId -> STM (ServerState, Text)
newNickState state clk conn tid = do
  s <- readTVar state
  let maybeNick = find (isNothing . flip lookupClient s) nicks
  case maybeNick of
    Nothing    -> error "can't find new nick"
    Just nick  -> do
      let s' = Client nick conn clk tid : s
      writeTVar state s'
      return (s', nick)
  where nicks = map (T.pack . ("anonymous"++) . show) ([0..] :: [Integer])

nickChangeState :: TVar ServerState -> Text -> Text -> STM (Either ServerState ServerState)
nickChangeState state oldNick newNick = do
  s <- readTVar state
  case lookupClient newNick s of
    Just _   -> return $ Left s
    Nothing  -> case lookupClient oldNick s of
                   Nothing    -> return $ Left s
                   Just targetClient  -> do
                     let s' = targetClient { clientNick = newNick } : filter ((/= oldNick) . clientNick) s
                     writeTVar state s'
                     return $ Right s'

disconnectState :: TVar ServerState -> Text -> STM (Maybe ServerState)
disconnectState state nick = do
  s <- readTVar state
  case lookupClient nick s of
    Nothing -> return Nothing
    Just _  -> do
      let s' = filter ((/= nick) . clientNick) s
      writeTVar state s'
      return $ Just s'

data Action = ActionSend WS.Connection DownMessage
            | ActionBroadcast DownMessage

runActions :: ServerState -> [Action] -> IO ()
runActions state = mapM_ runAction
  where runAction (ActionSend conn msg) = send conn msg
        runAction (ActionBroadcast msg) = broadcast state msg

talkAction :: TVar ServerState -> UTCTime -> Client -> UpMessage -> STM (ServerState, [Action], Text)
talkAction state clk Client { clientNick = nick, clientConnection = conn } m = updateActivity >> talkAction' m
  where updateActivity = do
          s <- readTVar state
          let s' = map f s
              f client  | clientNick client == nick  = client { clientActive = clk }
                        | otherwise                  = client
          writeTVar state s'
        talkAction' (UpTextMessage msg) = do
          s <- readTVar state
          return (s, [ ActionBroadcast $ TextMessage nick msg ], nick)
        talkAction' (UpPrivateMessage targetNick msg) = do
          s <- readTVar state
          let actions = case lookupClient targetNick s of
                          Nothing            -> [ ActionSend conn $ ErrorMessage $ "user " <> targetNick <> " doesn't exist" ]
                          Just targetClient  ->
                            [ ActionSend conn $ PrivateMessage nick msg -- echo to user
                            , ActionSend (clientConnection targetClient) $ PrivateMessage nick msg
                            ]
          return (s, actions, nick)
        talkAction' (UpNickMessage newNick) = do
          s <- nickChangeState state nick newNick
          case s of
            Left s'   -> return (s', [ ActionSend conn $ ErrorMessage "cannot change nick" ], nick)
            Right s'  -> do
              writeTVar state s'
              return (s', [ ActionBroadcast $ NickChangeMessage nick newNick ], newNick)
        talkAction' UpListMessage = do
          s <- readTVar state
          return (s, [ ActionSend conn $ UsersListMessage $ map clientNick s], nick)
        talkAction' UpPongMessage = do
          s <- readTVar state
          return (s, [], nick)

talk :: TVar ServerState -> Client -> IO ()
talk state client = handle catchDisconnect $ do
  dataMsg <- WS.receiveData $ clientConnection client
  case decode dataMsg :: Maybe UpMessage of
    Just msg  -> do
      clk <- getCurrentTime
      (s, actions, newNick) <- atomically $ talkAction state clk client msg
      runActions s actions
      talk state client { clientNick = newNick }
    Nothing   -> do
      send (clientConnection client) $ ErrorMessage "invalid input"
      talk state client
  where
    catchDisconnect :: SomeException -> IO ()
    catchDisconnect e = do
      s <- atomically $ disconnectState state $ clientNick client
      case s of
        Nothing  -> return ()
        Just s'  -> broadcast s' $ UserLeftMessage $ clientNick client
      putStrLn $ "exception: " ++ show e

pingInterval :: NominalDiffTime
pingInterval = 60

pingIntervalInt :: Int
pingIntervalInt = 60

pingTimeoutInterval :: NominalDiffTime
pingTimeoutInterval = 300

pinger :: TVar ServerState -> IO ()
pinger state = forever $ do
  -- wait
  threadDelay $ pingIntervalInt * 200000 -- one fifth
  clk <- getCurrentTime
  s <- readTVarIO state
  let pastClk         = addUTCTime (negate pingInterval) clk
      timeoutClk      = addUTCTime (negate pingTimeoutInterval) clk
      oldClients      = filter ((pastClk >) . clientActive) s
      timeoutClients  = filter ((timeoutClk >) . clientActive) s
  forM_ oldClients $ \client -> throwTo (clientThreadId client) WS.ConnectionClosed
  forM_ timeoutClients $ \client -> throwTo (clientThreadId client) WS.ConnectionClosed

staticApplication :: Application
staticApplication = staticApp $ embeddedSettings $(embedDir "static")
-- staticApplication = staticApp $ defaultWebAppSettings "static"

websocketsMiddleware :: TVar ServerState -> Middleware
websocketsMiddleware state = websocketsOr WS.defaultConnectionOptions $ websocketsApplication state

tsattiApplication :: TVar ServerState -> Application
tsattiApplication state = websocketsMiddleware state staticApplication
