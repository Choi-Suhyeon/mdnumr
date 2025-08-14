module Main (main) where

import Options.Applicative (execParser)

import CliOptions (CmdLineArgs(..), opts)
import Lib 
import Test

main :: IO ()
main = main' =<< execParser opts

main' :: CmdLineArgs -> IO ()
main' args = do
    print args

