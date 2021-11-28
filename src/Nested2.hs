{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Nested2 where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Time.Calendar ( Day )
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

ourUsers :: [User]
ourUsers = []

type UsersAPI =
  Get '[JSON] [User] -- list users
    :<|> ReqBody '[JSON] User :> PostNoContent -- add a user
    :<|> Capture "userid" Int
      :> ( Get '[JSON] User -- view a user
             :<|> ReqBody '[JSON] User :> PutNoContent -- update a user
             :<|> DeleteNoContent -- delete a user
         )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations
  where
    getUsers :: Handler [User]
    getUsers = error "..."

    newUser :: User -> Handler NoContent
    newUser = error "..."

    userOperations userid =
      viewUser userid :<|> updateUser userid :<|> deleteUser userid
      where
        viewUser :: Int -> Handler User
        viewUser = error "..."

        updateUser :: Int -> User -> Handler NoContent
        updateUser = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser = error "..."

type ProductsAPI =
  Get '[JSON] [Product] -- list products
    :<|> ReqBody '[JSON] Product :> PostNoContent -- add a product
    :<|> Capture "productid" Int
      :> ( Get '[JSON] Product -- view a product
             :<|> ReqBody '[JSON] Product :> PutNoContent -- update a product
             :<|> DeleteNoContent -- delete a product
         )

newtype Product = Product {productId :: Int} deriving (Eq, Show, Generic)

instance FromJSON Product

instance ToJSON Product

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations
  where
    getProducts :: Handler [Product]
    getProducts = error "..."

    newProduct :: Product -> Handler NoContent
    newProduct = error "..."

    productOperations productid =
      viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
      where
        viewProduct :: Int -> Handler Product
        viewProduct = error "..."

        updateProduct :: Int -> Product -> Handler NoContent
        updateProduct = error "..."

        deleteProduct :: Int -> Handler NoContent
        deleteProduct = error "..."

type CombinedAPI =
  "users" :> UsersAPI
    :<|> "products" :> ProductsAPI

server3 :: Server CombinedAPI
server3 = usersServer :<|> productsServer


combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

app3 :: Application
app3 = serve combinedAPI server3