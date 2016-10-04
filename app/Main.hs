{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (writeTVar, newTVar, readTVar)
import Control.Error.Util (hoistMaybe, isJustT)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Conduit (Source, (=$=), ($$))
import qualified Data.Conduit.Combinators as DC (concatMap, repeatM, mapM_)
import qualified Data.List as L (find, foldl1')
import Data.Monoid ((<>))
import Data.Text (pack)
import qualified Data.Text as T (take, drop, length)
import Hoogle (hoogle, HoogleResponse(..), HoogleResult(..))
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)
import Web.Telegram.API.Bot (Response(..)
                            , Token(..)
                            , getUpdates
                            , Update(..)
                            , sendMessageRequest
                            , sendMessage
                            , Message(..)
                            , Chat(..))


main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  token <- getToken
  updates token Nothing Nothing Nothing manager $$ DC.mapM_ (processUpdate token manager)


getToken :: IO Token
getToken = do
  tok <- lookupEnv "TELEGRAM_BOT_TOKEN"
  case tok of
    Just t ->
      return $ Token $ pack t
    Nothing ->
      error "Expected environment variable TELEGRAM_BOT_TOKEN"


updates :: (MonadIO m) => Token -> Maybe Int -> Maybe Int -> Maybe Int -> Manager -> Source m Update
updates token offset limit timeout manager = do
  offsetHolder <- liftIO $ atomically $ newTVar offset
  DC.repeatM (getUpdatesBatch offsetHolder) =$= DC.concatMap id
  where
    getUpdatesBatch offsetHolder = do
      currentOffset <- liftIO $ atomically $ readTVar offsetHolder
      resp <- liftIO $ getUpdates token currentOffset limit timeout manager
      case resp of
        Left e ->
          error $ show e
        Right (Response { result = batch }) -> do
          case batch of
            [] ->
              return []
            _ -> do
              let
                maxUpdateId =
                  maximum $ map update_id batch
                newOffset =
                  Just (maxUpdateId + 1)
              liftIO $ atomically $ writeTVar offsetHolder newOffset
              return batch


processUpdate :: (MonadThrow m, MonadIO m) => Token -> Manager -> Update -> m ()
processUpdate token manager update =
  void $ runMaybeT $ do
    msg <- hoistMaybe $ message update
    txt <- hoistMaybe $ text msg
    processed <- lift $ tryProcessCommand msg txt
    when (not processed) $ do
      sendReply msg "Привет!"
  where
    sendReply msg reply = do
      let
        chatId =
          pack $ show $ chat_id $ chat msg
        request =
          sendMessageRequest chatId reply
      void $ liftIO $ sendMessage token request manager      
    tryProcessCommand msg txt = isJustT $ do
      (cmd, handler) <- hoistMaybe $ L.find (checkCommand txt) commands
      let
        n =
          T.length cmd
        args =
          T.drop (n + 2) txt
      handler msg args
    checkCommand txt (cmd, _) =
      let
        n = T.length cmd
      in
        T.take (n + 2) txt == "/" <> cmd <> " "
    commands =
      [ ("hoogle", hoogleCmd)
      , ("hayoo", hayooCmd)
      ]
    hoogleCmd msg args = do
      HoogleResponse { results = res } <- hoogle args 0 5
      sendReply msg $ formatHoogleResults res
    formatHoogleResults =
      L.foldl1' (\x y -> x <> "  \n" <> y) . map (("1. " <>) . formatHoogleResult)
    formatHoogleResult res =
      self res <> "  \n" <> docs res <> "  \n" <> Hoogle.location res
    hayooCmd = undefined
