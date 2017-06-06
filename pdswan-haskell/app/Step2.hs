module Step2 where

import qualified Lib
import Lib (MalEnv(..), MalExp(..), MalReplError(..))
import qualified Data.Map as Map

main :: IO ()
main = Lib.loop $ Lib.malReadEvalPrint env
  where
    env :: MalEnv
    env = MalEnv $ Map.fromList [("+", add), ("-", sub), ("*", mul), ("/", div)]

    add :: [MalExp] -> Either MalReplError MalExp
    add [(MalNumber x), (MalNumber y)] = Right $ MalNumber (x + y)
    add _ = Left $ EvalError "Unexpected arguments to add"

    sub :: [MalExp] -> Either MalReplError MalExp
    sub [(MalNumber x), (MalNumber y)] = Right $ MalNumber (x - y)
    sub _ = Left $ EvalError "Unexpected arguments to sub"

    mul :: [MalExp] -> Either MalReplError MalExp
    mul [(MalNumber x), (MalNumber y)] = Right $ MalNumber (x * y)
    mul _ = Left $ EvalError "Unexpected arguments to mul"

    div :: [MalExp] -> Either MalReplError MalExp
    div [(MalNumber x), (MalNumber y)] = Right $ MalNumber (round ((fromIntegral x) / (fromIntegral y)))
    div _ = Left $ EvalError "Unexpected arguments to div"
