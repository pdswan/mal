module Step2 where

import qualified Lib
import Lib (MalEnv(..), MalExp(..), MalReplError(..))
import qualified Data.Map as Map

main :: IO ()
main = Lib.loop $ Lib.malReadEvalPrint env
  where
    env :: MalEnv
    env = MalEnv $ Map.fromList [("+", plus)]

    plus :: [MalExp] -> Either MalReplError MalExp
    plus [(MalNumber x), (MalNumber y)] = Right $ MalNumber (x + y)
    plus _ = Left $ EvalError "Unexpected arguments to plus"

