module Main where

import Network.Wai.Handler.Warp
import qualified Server
import qualified Api
import qualified LucidExample
import qualified Nested2
import qualified ReaderApi

main :: IO ()
main =
    -- run 8081 Server.app1
    -- run 8081 Api.app
    -- run 8081 LucidExample.app2
    -- run 8081 Nested2.app3
    run 8081 ReaderApi.app