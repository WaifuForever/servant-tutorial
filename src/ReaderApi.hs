{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ReaderApi where

import Control.Monad.Reader
import Data.Aeson (Value (Bool))
import Servant
import Data.Text.Internal.Read

type ReaderAPI =
  "a" :> Get '[JSON] Int
    :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

--boolToString :: Bool -> String
--boolToString True = "TRUE"
--boolToString False = "FALSE"

-- In this handler the string "hi" is stored 
readerToHandler :: Reader String a -> Handler a
readerToHandler r = return (runReader r "hi")

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

-- If in the request we receive a double then the function returns true
-- Otherwise it's going to return 1797
readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b
  where
    a :: Reader String Int
    a = return 1797

    b :: Double -> Reader String Bool
    b _ =  asks (== "hi")
 

readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI readerToHandler readerServerT

app :: Application 
app = serve readerAPI readerServer

-- curl http://localhost:8081/b -X GET -d '42.0' -H 'Content-Type: application/json'
-- curl http://localhost:8081/b -X GET -d '4' -H 'Content-Type: application/json'