-- A standard and slightly extensible UDP based server
-- By Ted Aronson

-- This server keeps track of its current active threads, which can be
-- queried by sending the message "ACTIVETHREADS".

-- The extensibility of this server depends on the inclusion of the
-- module CommandHandler, which must contain a function:

-- processMessage :: String -> TVar Integer -> IO()

-- For the current active threads functionality to work properly, it is
-- recommended that the user add 1 to the value of the TVar at the 
-- beginning of the function, and subtract 1 from the value of the TVar
-- at the end.

module ObjectServer where

import CommandHandler
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.Environment

-- The maximum number of bytes to read from incoming messages
maxMessageSize = 1000

startServer :: PortNumber -> IO()
startServer port = withSocketsDo $ do
	  openSocket <- socket AF_INET Datagram 0
	  bindSocket openSocket (SockAddrInet port iNADDR_ANY)
	  listenOn openSocket


-- Waits until a message is recieved on the socket.  Then creates a TVar that
-- will determine when the processManage thread is dead.  Creates the 
-- processMessage thread.  Unless the sender of the message is at the 
-- address tagged in the message, sends a message back when the processMessage
-- thread is done.  This reduces spam to the client.
listenOn :: Socket -> IO()
listenOn socket = do 
	(message, bytesRecieved, client) <- recvFrom socket maxMessageSize
	if message == "SHUTDOWN"
	 then do sClose socket
		 return ()
	 else do
		threadCounter <- atomically $ newTVar 1
		let chunkedMessage = words message
		originalRequester <- inet_addr (head chunkedMessage)
		clientAddress <- stripSockAddr client
		putStrLn ("Recieved \"" ++ message ++ "\" from " ++ 
				show clientAddress ++ ".  Original requester was " 
				++ show originalRequester)
		if (originalRequester == clientAddress)  -- This message is from the original requester
			then do forkIO $ processMessage message threadCounter
				listenOn socket

			else do forkIO $ processMessage message threadCounter
				forkIO $ respondWhenDead socket client threadCounter
				listenOn socket



-- Strips the hostname out of a SockAddr if possible.  Otherwise
-- returns ()
stripSockAddr :: SockAddr -> IO (HostAddress)
stripSockAddr (SockAddrUnix _) = return (1)
stripSockAddr (SockAddrInet _ address) = return address



-- Loops until isDead = 1, then sends a message to the operator confirming the 
-- dead thread.
respondWhenDead :: Socket -> SockAddr -> TVar Integer -> IO()
respondWhenDead send operator isDead = do
	dead <- atomically $ readTVar isDead
	if dead == 1
		then do sendTo send "THREAD STOP" operator
			return ()
		else do respondWhenDead send operator isDead