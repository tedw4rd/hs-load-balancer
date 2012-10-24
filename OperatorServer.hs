-- Asset Server
-- by Ted Aronson and Anna Filatova

module OperatorServer where

import Data.Map
import Network.Socket
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.IO.Unsafe
import System.Environment

-- The maximum number of bytes to read from incoming messages
maxMessageSize = 1000

childPortNum = 57357

data Server = MkServer HostAddress Socket 

instance Eq Server where
	(MkServer addr1 sock1) == (MkServer addr2 sock2) = addr1 == addr2 &&
							   sock1 == sock2
instance Ord Server where
	compare (MkServer addr1 _) (MkServer addr2 _) = compare addr1 addr2

instance Show Server where
	show (MkServer addr sock) = unsafePerformIO $ inet_ntoa addr

type ServerThreadMap = Map Server Integer

startServer :: [String] -> IO()
startServer serverArgs= withSocketsDo $ do
	  let serverAddresses = Prelude.map 
					(\x ->  unsafePerformIO $ inet_addr x) 
					serverArgs
	  serverList <- buildServerList serverAddresses 57358
	  let mapServerThreads = fromList $ Prelude.map 
						(\x -> (x,1)) 
						serverList
	  mapTVar <- atomically $ newTVar mapServerThreads

	  startChildSockets serverList mapTVar

	  inputChan <- atomically $ newTChan
	  inputSocket <- socket AF_INET Datagram 0
	  bindSocket inputSocket (SockAddrInet 57357 iNADDR_ANY)

	  forkIO $ listenForInput inputSocket inputChan

	  mainLoop inputChan mapTVar



buildServerList :: [HostAddress] -> PortNumber -> IO ([Server])
buildServerList [] _ = return []
buildServerList (addr:addrs) port = do 
	send <- socket AF_INET Datagram 0
	bindSocket send (SockAddrInet port iNADDR_ANY)
	otherServers <- buildServerList addrs (port+1)
	return ((MkServer addr send):otherServers)



-- Check if there are any new messages.  If so, find 
mainLoop :: TChan String -> TVar ServerThreadMap -> IO()
mainLoop input serverMap = do
	r <- atomically $ (Right `fmap` readTChan input)
				`orElse`
			  (Left `fmap` return())
	case r of
	  Left _ -> mainLoop input serverMap
	
	  Right req -> do let chunkedReq = words req
			  if head chunkedReq == "SHUTDOWN"
				then do putStrLn "Client Disconnected"
					mainLoop input serverMap
			  	else if chunkedReq !! 1 == "MAKE"
			   		then do servers <- atomically $ readTVar serverMap
				   		sendToAll (keys servers) req
				   		mainLoop input serverMap
			   		else do (MkServer addr sock) <- coldestServer serverMap
						putStrLn ("Sending \"" ++ req ++ "\" to " ++ show (MkServer addr sock) )
				   		sendTo sock req (SockAddrInet 57357 addr)
				   		atomically $ updateThreadValue serverMap 
					  				(MkServer addr sock) 1
				   		mainLoop input serverMap



-- Create new processes to listen to each child server.
startChildSockets :: [Server] -> TVar ServerThreadMap -> IO()
startChildSockets [] _ = return ()
startChildSockets (server:servers) serverMap = do
	forkIO $ listenToChild server serverMap
	startChildSockets servers serverMap



-- Listen for response from the child server.  Response should be
-- simply the number of active threads.  Attach that data to the
-- server's identifier and send it to the main loop.
listenToChild :: Server -> TVar ServerThreadMap -> IO()
listenToChild s@(MkServer host socket) serverMap = do
	(message, bytesRecieved, client) <- recvFrom socket maxMessageSize
	if message == "THREAD STOP"
		then do putStrLn (show s ++ " has stopped a thread")
			atomically $ updateThreadValue serverMap 
					(MkServer host socket) (-1)
			listenToChild s serverMap
		else do listenToChild s serverMap



-- Listen on the given socket for messages.
listenForInput :: Socket -> TChan String -> IO()
listenForInput sock chan = do 
	(message, bytesRecieved, client) <- recvFrom sock maxMessageSize
	atomically $ writeTChan chan message
	listenForInput sock chan


-- Strips the hostname out of a SockAddr if possible.  Otherwise
-- returns ()
stripSockAddr :: SockAddr -> IO (HostAddress)
stripSockAddr (SockAddrUnix _) = return (-1)
stripSockAddr (SockAddrInet _ address) = return address


-- Updates the server map by adding increment to the number of threads mapped
-- to each server.
updateThreadValue :: TVar ServerThreadMap -> Server -> Integer -> STM()
updateThreadValue serverMapVar server increment = do
	serverMap <- readTVar serverMapVar
	let updatedMap = adjust (+ increment) server serverMap
	writeTVar serverMapVar updatedMap


-- Finds the lowest element in the map (which corresponds to the lowest number
-- of threads on any server.  Then finds a server amongst the map that keys
-- that element.
coldestServer :: TVar ServerThreadMap -> IO(Server)
coldestServer mapTVar = do 
	serverMap <- atomically $ readTVar mapTVar
	let minThreads = head $ elems serverMap
	let coldServers = Data.Map.filter (\x -> x == minThreads) serverMap
	return $ fst $ head $ toList coldServers


-- Sends a message out to every child server in this group

sendToAll :: [Server] -> String -> IO()
sendToAll [] _ = return ()
sendToAll ((MkServer addr sock):servers) message = do
	sendTo sock message (SockAddrInet 57357 addr)
	sendToAll servers message

