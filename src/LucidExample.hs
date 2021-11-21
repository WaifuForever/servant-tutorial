{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module LucidExample where

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
import Servant.HTML.Lucid
import Servant.Types.SourceT (source)
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

type PersonAPI =
  "persons"
    :> Get '[JSON, HTML] [Person]

data Person = Person
  { firstName :: String,
    lastName :: String
  }
  deriving (Generic)

instance ToJSON Person

instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml

people :: [Person]
people = 
    [
        Person "Isaac" "Newton",
        Person "Benjamin" "Franklin"
    ]
personAPI :: Proxy PersonAPI
personAPI = Proxy 

server4 :: Server PersonAPI
server4 = return people

app2 :: Application 
app2 = serve personAPI server4
