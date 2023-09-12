{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redis where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Except (ExceptT, MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (putStrLn)
import Data.Map (Map)
import Data.Map qualified as Map
import Network.Simple.TCP (Socket, recv, send)
import RESP (Resp (..), decode, encode)
import Text.Parsec (ParseError)
import Utils (toBSSlow)
import Prelude hiding (putStrLn)

type Key = ByteString
type Val = ByteString
type StoredVal = ByteString
type Expiry = Integer

type DB = TVar (Map Key StoredVal)

data RedisError
    = ParsingError ParseError
    | EncodingError ByteString
    | UnimplementedError ByteString
    | EmptyBuffer
    deriving (Show)

newtype Redis a = Redis {runRedis :: ReaderT DB (ExceptT RedisError IO) a}
    deriving (Functor, Applicative, Monad, MonadError RedisError, MonadIO, MonadReader DB)

-- Network handling

bufferSize :: Int
bufferSize = 1024

clientLoop :: Socket -> Redis ()
clientLoop socket = do
    liftIO $ putStrLn "\nHandling Client"
    redisCmdStr <- recv socket bufferSize
    redisCmdStr <- maybe (throwError EmptyBuffer) pure redisCmdStr
    liftIO $ print $ "Redis command:" <> redisCmdStr
    result <- runCmdStr redisCmdStr
    send socket result
    clientLoop socket

-- Redis Command execution

runCmd :: Resp -> Redis Resp
runCmd (Array 1 [BulkStr "ping"]) = pure $ Str "PONG"
runCmd (Array 2 [BulkStr "echo", BulkStr xs]) = pure $ Str xs
runCmd _ = throwError $ UnimplementedError ""

runCmdStr :: ByteString -> Redis ByteString
runCmdStr redisCmdStr = do
    redisCmd <- liftEither $ first ParsingError $ decode redisCmdStr
    liftIO $ putStrLn $ "Parse Success: " <> toBSSlow redisCmd
    result <- runCmd redisCmd
    liftIO $ putStrLn $ "Result: " <> toBSSlow result
    liftEither $ first EncodingError $ encode result

-- Cache based on a Map data structure

newRedis :: IO DB
newRedis = newTVarIO Map.empty
