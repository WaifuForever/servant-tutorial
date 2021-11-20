module Main where

import Network.Wai.Handler.Warp
import qualified Server
import qualified Api

main :: IO ()
main =
    -- run 8081 app1
    run 8081 Api.app