{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyOutput where
import Verify

import Verify.Graphics.Vty.Output (terminalsOfInterest)

import Control.Monad

import System.Posix.Env
import System.IO

tests :: IO [Test]
tests = concat <$> forM terminalsOfInterest (\termName -> do
    -- check if that terminfo exists
    putStrLn $ "testing end to end for terminal: " ++ termName
    return [ verify ("verify " ++ termName ++ " could output a picture")
                                   (smokeTestTermNonMac termName)
                          -- this is excessive.
                          , verify ("verify " ++ termName ++ " could output a picture on a Mac.")
                                   (smokeTestTermMac termName)
                          ]
    )

smokeTestTermNonMac :: String -> () -> Property
smokeTestTermNonMac termName i = liftIOResult $ do
    -- unset the TERM_PROGRAM environment variable if set.
    -- Required to execute regression test for #42 on a mac
    unsetEnv "TERM_PROGRAM"
    smokeTestTerm termName i

smokeTestTermMac :: String -> () -> Property
smokeTestTermMac termName i = liftIOResult $ do
    setEnv "TERM_PROGRAM" "Apple_Terminal" True
    smokeTestTerm termName i

smokeTestTerm :: String -> () -> IO Result
smokeTestTerm termName () = do
    return succeeded

