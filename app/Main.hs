{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Server (app, Env(..), runK)
import Katip (initLogEnv, Environment(..), Namespace(..))
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portEnv >>= readMaybe)
  putStrLn $ "Running server on port " ++ show port
  env <- Katip.initLogEnv (Namespace ["MyApp"]) (Environment "production")
  runK env "Start process"
  run port $ app (Env env)
