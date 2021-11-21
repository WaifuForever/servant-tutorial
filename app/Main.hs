module Main where

import Network.Wai.Handler.Warp
import qualified Server
import qualified Api
import qualified LucidExample

main :: IO ()
main =
    -- run 8081 Server.app1
    -- run 8081 Api.app
    run 8081 LucidExample.app2