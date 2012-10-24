-- Asset Server
-- by Ted Aronson and Anna Filatova

-- This module defines the Object and the search operations on the Object.


module ObjectHandler where

import System.IO
import System.IO.Error
import Data.List

type Object = (Id, Name, Position, Size, Shape, Creator)


data Condition = IdIs Int 
               | NameHas String
               | All
               
type Id = Int
type Name = String
type Position = (Float, Float, Float)
type Size = (Float, Float, Float)
type Shape = String
type Creator = Int

-- Each Object is stored in a container when created. After creation the
-- Object has to stay in the container where it was created, it cannot
-- be moved or deleted.
type Container = FilePath


-- Add Object to a container.
addObject :: Container -> Object -> IO ()
addObject container obj = do handle <- openFile container AppendMode
			     hPutStrLn handle (convertObjectToString obj)
			     hClose handle
-- Find Object in a specified container based on a condition. 
-- If the goal is to find an object with a specified id:
findObject :: Container -> Condition -> IO [Object]
findObject container (IdIs idd) = 
	do handle <- openFile container ReadMode
	   objects <- toListOfObjects $ searchForID handle (show idd) 0
	   hClose handle
	   return objects

  -- If the goal is to find an object which name contains a specified string:                                  
findObject container (NameHas name) = 
  do handle <- openFile container ReadMode
     objects <-toListOfObjects $ searchForName handle name 1
     hClose handle
     return objects

-- If the goal is to get the listing of all objects in the container.
findObject container All = findObject container (NameHas "")

searchForID :: Handle -> String -> Int -> IO [String]
searchForID fileContents property ind = 
	do pulled <- try (hGetLine fileContents)
	   case pulled of
	    Left error | isEOFError error
	      -> do return []
		       | otherwise
	      -> return ["-1 ERROR (-1,-1,-1) (-1,-1,-1) ERROR -1"]
	    Right line
	      -> do restOfFile <- searchForID fileContents property ind
 	            if ((words line) !! ind) == property
		     then return $ line : restOfFile
		     else return $ restOfFile
		     
searchForName :: Handle -> String -> Int -> IO [String]
searchForName fileContents property ind = 
	do pulled <- try (hGetLine fileContents)
	   case pulled of
	    Left error | isEOFError error
	      -> do return []
		       | otherwise
	      -> do return ["-1 ERROR (-1,-1,-1) (-1,-1,-1) ERROR -1"]
	    Right line
	      -> do restOfFile <- searchForName fileContents property ind
 	            if property `isInfixOf` ((words line) !! ind)
		     then return $ line : restOfFile
		     else return $ restOfFile

---------------------------------------------
-- Conversion Functions and Helper Methods --
---------------------------------------------


convertObjectToString :: Object -> String
convertObjectToString (a,b,c,d,e,f) = 
	show a ++ " " ++ b ++ " " ++ show c ++ " "
	++ show d ++ " " ++ e ++ " " ++ show f


-- To make working with the file easier, it is chopped in separate lines.
chopFile :: IO String -> IO [String]
chopFile strIO = 
         do str <- strIO
            return (lines str)


toListOfObjects :: IO [String] -> IO [Object]
toListOfObjects listIO = 
           do list <- listIO
              return $ map listToObject (map words list)

-- One line (already broken down into separate characteristics of an object) is converted to Object type.
listToObject :: [String] -> Object
listToObject message = if length message /= 6	 -- return an error Objects
			then (-1, "ERROR", (-1,-1,-1), (-1,-1,-1), "ERROR", -1)
			else (read (message !! 0):: Int, message !! 1,
		       	      read (message !! 2):: (Float, Float, Float), 
		       	      read (message !! 3):: (Float, Float, Float), 
		       	      message !! 4, read (message !! 5):: Int) 
	       	 
