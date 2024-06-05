module Main where

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Server (app)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = maybe 8080 id (portEnv >>= readMaybe)
  putStrLn $ "Running server on port " ++ show port
  run port app
