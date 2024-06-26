{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe               (fromMaybe)
import           Prelude
import           Katip                    (Environment(..), Namespace(..), initLogEnv)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (lookupEnv)
import           Text.Read                (readMaybe)

import           ZkFold.Prover.API.Server (app, Env(..), runK)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portEnv >>= readMaybe)
  putStrLn $ "Running server on port " ++ show port
  env <- Katip.initLogEnv (Namespace ["MyApp"]) (Environment "production")
  runK env "Start process"
  run port $ app (Env env)
