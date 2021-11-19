{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Server where

import ApiType (UserAPI10)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time (UTCTime)
import Data.Time.Calendar
import GHC.Generics
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
import GHC.Base (Symbol)

type UserAPI (name :: Symbol) = 
    name
    :> Get '[JSON] User


type UsersAPI =
  "users"
    :> Get '[JSON] [User]

type UsersAPI1 =
    UsersAPI    
    :<|> UserAPI "albert"    
    :<|> UserAPI "isaac" 
    
    
isaac :: User
isaac = User "Isaac Newton" 338 "issac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 116 "ae@mc2.org" (fromGregorian 1905 12 1)

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

ourUsers :: [User]
ourUsers =
  [ isaac,
    albert
  ]

server :: Server UsersAPI
server = return ourUsers

server1 :: Server UsersAPI1
server1 =
  return ourUsers
    :<|> return albert
    :<|> return isaac

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

usersAPI1 :: Proxy UsersAPI1
usersAPI1 = Proxy

app :: Application
app = serve usersAPI server

app1 :: Application
app1 = serve usersAPI1 server1