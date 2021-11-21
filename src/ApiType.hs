{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

-- GET /users/?sortby={age, name}
-- return a list of JSON objects describind user
-- with fields age, name, email, registration_date

type UserAPI =
  "users"
    :> QueryParam "sortby" SortBy
    :> Get '[JSON] [User]

-- If we wanted a root endpoint, simply don't use any
-- ':<' example:
-- type RootEndpoint = Get '[JSON] [User]

-- Either /users/list-all
-- or     /list-all/users
type UserAPI2 =
  "users"
    :> "list-all"
    :> Get '[JSON] [User]
    :<|> "list-all"
    :> "users"
    :> Get '[JSON] [User]

-- /user/:userid
-- except that we explicitly say that "userid" must be an integer
type UserAPI5 =
  "users"
    :> Capture "userid" Integer
    :> Get '[JSON] User
    :<|> "users"
    :> Capture "userid" Integer
    :> DeleteNoContent '[JSON] NoContent -- equivalent to 'DELETE /user/:userid'

-- GET /users/?sortby={age, name}
type UserAPI6 =
  "users"
    :> QueryParam "sortby" SortBy
    :> Get '[JSON] [User]

-- POST /users
-- PUT /users/:userid
type UserAPI7 =
  "users"
    :> ReqBody '[JSON] User
    :> Post '[JSON] User
    :<|> "users"
    :> Capture "userid" Integer
    :> ReqBody '[JSON] User
    :> Put '[JSON] User

type UserAPI8 =
  "users"
    :> Header "User-Agent" Text
    :> Get '[JSON] [User]

type UserAPI10 =
  "users"
    :> Get
         '[JSON]
         (Headers '[Header "User-Count" Integer] [User])

type ProtectedAPI11 =
  UserAPI
    :<|> BasicAuth "my-realm" User
    :> UserAPI2

-- embedded api
type SubPathAPi =
  "users"
    :> (UserAPI2 :<|> UserAPI8)
    :> Get '[JSON] User

type UserAPI12 innerAPI =
  UserAPI
    :<|> "inner" :> innerAPI

type UserAPI12Alone = UserAPI12 EmptyAPI

type UserAPI13 =
  "users"
    :> Get '[JSON] [User]
    :<|> Raw

data SortBy = Age | Name

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: UTCTime
  }