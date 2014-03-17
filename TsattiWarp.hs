import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setTimeout)

import Tsatti (tsattiApplication, pinger)

import Control.Concurrent
import Control.Concurrent.STM

port :: Int
port = 3000

timeout :: Int
timeout = 300 -- 5 minutes

main :: IO ()
main = do
  putStrLn $ "Starting server at port " ++ show port
  state <- newTVarIO []
  _ <- forkIO $ pinger state
  runSettings s $ tsattiApplication state
    where s = setTimeout timeout $ setPort port defaultSettings
