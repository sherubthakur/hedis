{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redis where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Except (ExceptT, MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
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
runCmd (Array 3 [BulkStr "set", BulkStr k, BulkStr v]) = do
    _ <- rInsert k v
    pure $ Str "OK"
runCmd (Array 2 [BulkStr "get", BulkStr k]) = do
    mval <- rLookup k
    pure $ maybe NullBulk Str mval
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

rInsert :: Key -> Val -> Redis ()
rInsert k value = do
    db <- ask
    liftIO $ atomically $ modifyTVar db (Map.insert k value)

rLookup :: Key -> Redis (Maybe Val)
rLookup k = do
    db <- ask
    db <- liftIO $ readTVarIO db
    let value = Map.lookup k db
    pure value
