{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Nested1 where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Prelude.Compat (Eq, Int, Show, String, error, return, undefined)
import Servant
  ( Application,
    Capture,
    DeleteNoContent,
    Get,
    Handler,
    JSON,
    NoContent,
    PostNoContent,
    Proxy (..),
    PutNoContent,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Prelude ()

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

-- Each handler receives the userid argument
type UserAPI =
  Capture "userid" Int :> Get '[JSON] User
    :<|> Capture "userid" Int :> DeleteNoContent

-- Same as UserAPI but condensed, the whole server takes the userid
type UserAPI2 =
  Capture "userid" Int
    :> ( Get '[JSON] User
           :<|> DeleteNoContent
       )

-- Server UserAPI3 = (Int -> Handler User)
--                     :<|> (Int -> Handler NoContent)

-- Server UserAPI4 = (Int -> Handler User)
--                     :<|> (Int -> Handler NoContent)

server :: Server UserAPI
server = getUser :<|> deleteUser
  where
    getUser :: Int -> Handler User
    getUser _userid = undefined

    deleteUser :: Int -> Handler NoContent
    deleteUser _userid = undefined

server2 :: Server UserAPI2
server2 userid = getUser userid :<|> deleteUser userid
  where
    getUser :: Int -> Handler User
    getUser = undefined

    deleteUser :: Int -> Handler NoContent
    deleteUser = undefined

-- GET /users
-- GET /users/:userid
type API1 =
  "users"
    :> ( Get '[JSON] [User] --user listing
           :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular User
       )

type API2 =
  ReqBody '[JSON] User
    :> ( Get '[JSON] User --just display the same user back, don't register it
           :<|> PostNoContent -- register the user, empty response
       )