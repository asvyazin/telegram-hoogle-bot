{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Hoogle (hoogle, HoogleResponse(..), HoogleResult(..)) where


import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple (Request, parseRequest, httpJSON, getResponseBody)
import Network.HTTP.Types.URI (renderSimpleQuery)


data HoogleResult =
  HoogleResult
  { location :: Text
  , self :: Text
  , docs :: Text
  }


$(deriveJSON defaultOptions ''HoogleResult)


data HoogleResponse =
  HoogleResponse
  { version :: Text
  , results :: [HoogleResult]
  }


$(deriveJSON defaultOptions ''HoogleResponse)


hoogle :: (MonadThrow m, MonadIO m) => Text -> Int -> Int -> m HoogleResponse
hoogle query start count = do
  req <- hoogleRequest (encodeUtf8 query) start count
  getResponseBody <$> httpJSON req


hoogleRequest :: (MonadThrow m) => ByteString -> Int -> Int -> m Request
hoogleRequest queryText start count = do
  let
    qs =
      [ ("mode", "json")
      , ("hoogle", queryText)
      , ("start", pack (show start))
      , ("count", pack (show count))
      ]
  parseRequest $ unpack $ hoogleBaseUrl <> renderSimpleQuery True qs


hoogleBaseUrl :: ByteString
hoogleBaseUrl = "http://www.haskell.org/hoogle/"
