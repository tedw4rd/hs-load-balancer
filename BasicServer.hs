module Main where

import ObjectServer
import System.Environment

main =  do [portNum] <- getArgs
	   let port = fromIntegral (read portNum :: Int)
	   startServer port