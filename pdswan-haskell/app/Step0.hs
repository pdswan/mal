module Step0 where

import qualified Lib

main :: IO ()
main = Lib.loop echo
  where
    echo :: String -> String
    echo = id

