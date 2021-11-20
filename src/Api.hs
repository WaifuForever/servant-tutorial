{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import ApiType (User (name), UserAPI10)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString, intercalate)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time (UTCTime)
import Data.Time.Calendar
import GHC.Base (Symbol)
import GHC.Generics
import GHC.Num (Integer)
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Prelude.Compat
import Servant
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8
import Prelude ()

-- GET /position/:x/:y
-- GET /hello/?name={String}
--

type API =
    "position"
    :> Capture "x" Integer
    :> Capture "y" Integer
    :> Get '[JSON] Position
    :<|> "hello"
    :> QueryParam "name" String
    :> Get '[JSON] HelloMessage
    :<|> "marketing"
    :> ReqBody '[JSON] ClientInfo
    :> Post '[JSON] Email

data Position = Position
  { xCoordinate :: Integer,
    yCoordinate :: Integer
  }
  deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
  deriving (Generic)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String,
    clientEmail :: String,
    clientAge :: Integer,
    clientInterestedIn :: [String]
  }
  deriving (Generic)

instance FromJSON ClientInfo

instance ToJSON ClientInfo

data Email = Email
  { from :: String,
    to :: String,
    subject :: String,
    body :: String
  }
  deriving (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.org"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
      "Hi " ++ clientName c ++ ",\n\n"
        ++ "Since you've recently turned "
        ++ show (clientAge c)
        ++ ", have you checked out ou latest"
        ++ Data.List.intercalate ", " (clientInterestedIn c)
        ++ "products? Give us a visit!"

server :: Server API
server =
  position
    :<|> hello
    :<|> marketing
  where
    position :: Integer -> Integer -> Handler Position
    position x y = return $ Position x y

    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $
      case mname of
        Nothing -> "Hello, anonymous coward!"
        Just n -> "Hello, " ++ n

    marketing :: ClientInfo -> Handler Email
    marketing clientInfo = return $ emailForClient clientInfo

apiProxy :: Proxy API
apiProxy = Proxy 

app :: Application
app = serve apiProxy server