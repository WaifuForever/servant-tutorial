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

data SortBy = Age | Name

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: UTCTime
  }