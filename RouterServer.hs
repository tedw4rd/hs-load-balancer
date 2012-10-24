module Main where

import OperatorServer
import System.Environment

main = do portNums <- getArgs
	  startServer portNums