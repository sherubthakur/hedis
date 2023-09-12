{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (putStrLn)
import Network.Simple.TCP (HostPreference (HostAny), closeSock, serve)
import Redis (RedisError (EmptyBuffer), clientLoop, newRedis, runRedis)
import Utils (toBSSlow)
import Prelude hiding (putStrLn)

main :: IO ()
main = do
    putStrLn "Logs from your program will appear here\n"

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " <> toBSSlow port
    redis <- newRedis
    serve HostAny port $ \(socket, address) -> do
        putStrLn $ "successfully connected client: " <> toBSSlow address
        errRes <- runExceptT (runReaderT (runRedis $ clientLoop socket) redis)
        case errRes of
            Left EmptyBuffer -> pure ()
            Left err ->
                putStrLn $
                    "Error on client connection : "
                        <> toBSSlow address
                        <> "\nError:"
                        <> toBSSlow err
            Right () -> pure ()
        closeSock socket

