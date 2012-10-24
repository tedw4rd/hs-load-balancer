
---------------------------------------------------------
-- Asset Server
-- Ted Aronson and Anna Filatova
--
-- Connection Handler Module
-- Deals with network connections
--
-- When the asset server recieves a connection from a client,
-- this module works with the outputted IO Handle to read
-- messages, execute the appropriate read/write commands, and 
-- reports the result back to the client that accessed the
-- server.  It does so through the use of ForkOS's which 
-- allow several commands to be executed at once.
--
-- The main function in this module is the processRequest
-- function.  processRequest accepts an IO Handle to a
-- network connection and reads its contents.  It
-- immediately forks off a new process to deal with the
-- request.  These processes are the results of findObject
-- and storeObject, both of which use the STM monad to
-- execute the desired action, and then send a response to
-- the client
---------------------------------------------------------

module CommandHandler (processMessage) where

import ObjectHandler
import Network.Socket
import Control.Concurrent.STM
import Data.Word

-- A simple datatype for commands
data Command =   Write Container Object
	       | Find Container Condition
	       | BadMessage

-- The client's IP Address
type ClientAddress = Word32

-- The port on the client's side that listens for messages
clientPort = 57357
sendingPort = 57356


-- The main function of the module.  See top of the file
-- for more details
processMessage:: String -> TVar Integer -> IO ()
processMessage message threadCounter = do
		    atomically $ do threads <- readTVar threadCounter
				    writeTVar threadCounter (threads+1)
		    let chunkedMessage = words message
		    returnAddress <- inet_addr (chunkedMessage !! 0)
		    command <- convertToCommand $ unwords (tail chunkedMessage)
		    result <- executeCommand command
		    sendMessage result returnAddress
		    atomically $ do threads <- readTVar threadCounter
				    writeTVar threadCounter (threads-1)

-- Creates a new object in a specified container. Return a message confirming 
-- the successful creation.
executeCommand :: Command -> IO String
executeCommand (Write con obj) = 
		do addObject con obj
		   return "Object Created"

-- Searches for obects that satisfy the specified condition. Returns the list
-- of all object found.
executeCommand (Find container condition) =
		do objects <- findObject container condition
		   printedList <- (return (foldr (++) "" (map convertObjectToString objects)))
		   return $ "Found: " ++ printedList

-- For any command other than MAKE or FIND, an error is returned and no
-- object is created.
executeCommand _ = do return "ERROR: INVALID COMMAND!"



---------------------------------------------
-- Conversion Functions and Helper Methods --
---------------------------------------------


-- Converts a string to a command in the IO monad
-- If the string is malformed, returns an ioError
convertToCommand :: String -> IO Command
convertToCommand message = let chunkedMessage = words message
			   in do
			       if(head chunkedMessage == "MAKE")
			        -- message is a write command
				then do container <- (return (chunkedMessage !! 1))
				        return (Write container (listToObject (tail(tail chunkedMessage))))
				else if (head chunkedMessage == "GET")
			              then return (Find (chunkedMessage !! 1)
					     (listToCondition (tail(tail chunkedMessage))))
				else
				 return BadMessage
-- Defines what search is requested (search for an ID, search for a name,
-- or search for all objects.
listToCondition :: [String] -> Condition
listToCondition words = case head words of
				"NAMEHAS" -> NameHas (head (tail words))
				"IDIS" -> IdIs (read (head (tail words))::Int)
				_ -> All


-- A simple UDP sending method
-- Open a UDP INet socket and bind it to the sending port.  
-- Then, send the specified message to the port clientPort at 
-- the specified address.  Finally, close the socket.
-- clientPort is defined at the top of this file.
sendMessage :: String -> ClientAddress -> IO()
sendMessage message address = do 
	sendingSocket <- socket AF_INET Datagram 0 
	bindSocket sendingSocket (SockAddrInet sendingPort iNADDR_ANY)
	sendTo sendingSocket message (SockAddrInet clientPort address)
	sClose sendingSocket

