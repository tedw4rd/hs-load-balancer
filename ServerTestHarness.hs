module ServerTestHarness (testServer, Test) where

import System
import System.CPUTime
import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket

type Input = String
type Output = String
type Test = (Input, (Output -> IO(Bool)))

-- The maximum number of bytes to read from incoming messages
maxMessageSize = 1000


testServer :: HostAddress -> PortNumber -> [Test] -> IO ()
testServer server targetPort tests = do
	sendingSocket <- socket AF_INET Datagram 0
	bindSocket sendingSocket (SockAddrInet 57356 iNADDR_ANY)
	listenSocket <- socket AF_INET Datagram 0
	bindSocket listenSocket (SockAddrInet 57357 iNADDR_ANY)
	startTests tests sendingSocket listenSocket (SockAddrInet targetPort server) 
	sClose sendingSocket
	sClose listenSocket

startTests :: [Test] -> Socket -> Socket -> SockAddr -> IO ()
startTests tests send listen target = do 
	messageChannel <- atomically $ newTChan
	listenThread <- forkIO $ listenLoop listen messageChannel
	runTests tests send target messageChannel
	killThread listenThread

runTests :: [Test] -> Socket -> SockAddr -> TChan String -> IO ()
runTests [] s target _ = do sendTo s "SHUTDOWN" target
		            return ()
runTests ((i,f):tests) send target channel = do
	sendTo send i target
	startTime <- getCPUTime
	(success, value) <- evaluateTest f channel
	endTime <- getCPUTime
	let testTime = (endTime - startTime) `div` 1000000000
	if success
		then appendFile "./TestResults.txt" ("Test Passed in " ++ show testTime ++ " ms.  Returned " ++ value ++ "\n")
		else appendFile "./TestResults.txt" ("Test Failed in " ++ show testTime ++ " ms.  Returned " ++ value ++ "\n")
	runTests tests send target channel

evaluateTest :: (Output -> IO (Bool)) -> TChan String -> IO (Bool, String)
evaluateTest isCorrect channel = do
	r <- atomically $ (Right `fmap` readTChan channel) `orElse` (Left `fmap` return ())
	
	case r of
		Right s -> do
			success <- isCorrect s
			if success
				then return (True, s)
				else return (False, s)
		
		Left _ -> evaluateTest isCorrect channel

listenLoop :: Socket -> TChan String -> IO()
listenLoop socket channel = do 
	(message, bytesRecieved, client) <- recvFrom socket maxMessageSize
	atomically $ writeTChan channel message
	listenLoop socket channel