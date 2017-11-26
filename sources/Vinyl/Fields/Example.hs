{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Vinyl.Fields.Example where
import Vinyl.Fields()
import System.Environment

{-|
@
stack build && stack exec -- example-vinyl-fields
@
-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

mainWith s = do
 putStrLn s
 putStrLn "(Vinyl.Fields.Example...)"

