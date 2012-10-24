-- Asset Server
-- by Ted Aronson and Anna Filatova

-- This module represents a test to verify how the server is handling
-- the requests.

module ConnectionTester where

import ServerTestHarness
import Network.Socket

-- This function takes the correct (expected) answer and generates a function
-- that returns true if this correct answer matches the actual answer receved
-- after the request was executed.
generateValidResponse :: String -> (String -> IO Bool)
generateValidResponse = validResponse

validResponse :: String -> String -> IO Bool
validResponse correct string = return (correct == string)

-- Testing that the generating function works properly.
testGVR = snd ("MAKE something", generateValidResponse "Object Created") 
 "Object Created"

-- Executes all the tests.
main = do hostAddress <- inet_addr "158.130.59.25"
	  testServer hostAddress 57357 allTests

--------------------------------------------------------------------------------
--TESTS:
allTests = 
 [("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 1 glasses (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 2 wallet (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 3 phone (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 6 house1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 7 house2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 8 house3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 CREATE ./AssetServerStorage/bag.txt 1 cat (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "ERROR: INVALID COMMAND!")
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 1", generateValidResponse (foldr (++) "" ("Found: " : ["1 glasses (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 3", generateValidResponse (foldr (++) "" ("Found: " : ["3 phone (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 5", generateValidResponse (foldr (++) "" ("Found: " : ["5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 CREATE ./AssetServerStorage/bag.txt 2 dog (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "ERROR: INVALID COMMAND!")
 ,("158.130.58.218 CREATE ./AssetServerStorage/bag.txt 3 cow (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "ERROR: INVALID COMMAND!")
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt NAMEHAS g", generateValidResponse (foldr (++) "" ("Found: " : ["1 glasses (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt NAMEHAS wall", generateValidResponse (foldr (++) "" ("Found: " : ["2 wallet (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS chair", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt NAMEHAS e", generateValidResponse (foldr (++) "" ("Found: " : ["1 glasses (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","2 wallet (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","3 phone (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 9 CD (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 10 lunch (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 12 house4 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["6 house1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","7 house2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","8 house3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","12 house4 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["1 glasses (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","2 wallet (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","3 phone (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","9 CD (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","10 lunch (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS bed", generateValidResponse (foldr (++) "" ("Found: " : ["11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt NAMEHAS 4", generateValidResponse (foldr (++) "" ("Found: " : ["12 house4 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 12", generateValidResponse (foldr (++) "" ("Found: " : ["12 house4 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 3", generateValidResponse (foldr (++) "" ("Found: " : ["3 phone (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 10", generateValidResponse (foldr (++) "" ("Found: " : ["10 lunch (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 1", generateValidResponse (foldr (++) "" ("Found: " : [])))
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 13 keys (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 14 picture1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 15 picture2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 16 fence (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 17 desk (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 18 bridge1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 19 bridge2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 20 picture3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 TRY ./AssetServerStorage/bag.txt 5 dog (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "ERROR: INVALID COMMAND!")
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt NAMEHAS pic", generateValidResponse (foldr (++) "" ("Found: " : ["14 picture1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","15 picture2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","20 picture3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt NAMEHAS e2", generateValidResponse (foldr (++) "" ("Found: " : ["7 house2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","19 bridge2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS e", generateValidResponse (foldr (++) "" ("Found: " : ["11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","17 desk (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 4", generateValidResponse (foldr (++) "" ("Found: " : [])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 13", generateValidResponse (foldr (++) "" ("Found: " : ["13 keys (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 17", generateValidResponse (foldr (++) "" ("Found: " : ["17 desk (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 6", generateValidResponse (foldr (++) "" ("Found: " : ["6 house1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 9", generateValidResponse (foldr (++) "" ("Found: " : ["9 CD (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 15", generateValidResponse (foldr (++) "" ("Found: " : ["15 picture2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 18", generateValidResponse (foldr (++) "" ("Found: " : ["18 bridge1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","17 desk (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 21 shelf1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 22 shelf2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 23 computer (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 22", generateValidResponse (foldr (++) "" ("Found: " : ["22 shelf2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 19", generateValidResponse (foldr (++) "" ("Found: " : ["19 bridge2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 21", generateValidResponse (foldr (++) "" ("Found: " : ["21 shelf1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","17 desk (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","21 shelf1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","22 shelf2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","23 computer (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 4", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS shelf", generateValidResponse (foldr (++) "" ("Found: " : ["21 shelf1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","22 shelf2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 11", generateValidResponse (foldr (++) "" ("Found: " : ["11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 24 picture4 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 25 pen (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 26 redCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 27 blueCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/bag.txt 28 pencil (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 29 book1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 30 greenCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 31 blackCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 26", generateValidResponse (foldr (++) "" ("Found: " : ["26 redCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 30", generateValidResponse (foldr (++) "" ("Found: " : ["30 greenCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","17 desk (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","21 shelf1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","22 shelf2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","23 computer (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","29 book1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 29", generateValidResponse (foldr (++) "" ("Found: " : ["29 book1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS b", generateValidResponse (foldr (++) "" ("Found: " : ["11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","29 book1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 23", generateValidResponse (foldr (++) "" ("Found: " : ["23 computer (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["6 house1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","7 house2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","8 house3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","12 house4 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","16 fence (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","18 bridge1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","19 bridge2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","26 redCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","27 blueCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","30 greenCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","31 blackCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 FIND ./AssetServerStorage/street.txt ALL", generateValidResponse "ERROR: INVALID COMMAND!")
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt NAMEHAS pen", generateValidResponse (foldr (++) "" ("Found: " : ["25 pen (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","28 pencil (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt NAMEHAS Car", generateValidResponse (foldr (++) "" ("Found: " : ["26 redCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","27 blueCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","30 greenCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","31 blackCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS book", generateValidResponse (foldr (++) "" ("Found: " : ["29 book1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 29", generateValidResponse (foldr (++) "" ("Found: " : [])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 28", generateValidResponse (foldr (++) "" ("Found: " : ["28 pencil (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 5", generateValidResponse (foldr (++) "" ("Found: " : ["5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 30", generateValidResponse (foldr (++) "" ("Found: " : ["30 greenCar (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 32 mirror (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 33 closet (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/room.txt 34 chair3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 35 person1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 36 person2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 MAKE ./AssetServerStorage/street.txt 37 person3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11", generateValidResponse "Object Created")
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 22", generateValidResponse (foldr (++) "" ("Found: " : ["22 shelf2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 19", generateValidResponse (foldr (++) "" ("Found: " : ["19 bridge2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 21", generateValidResponse (foldr (++) "" ("Found: " : ["21 shelf1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt ALL", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","11 bed (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","17 desk (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","21 shelf1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","22 shelf2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","23 computer (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","29 book1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","32 mirror (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","33 closet (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","34 chair3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 34", generateValidResponse (foldr (++) "" ("Found: " : ["34 chair3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS chair", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","34 chair3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt NAMEHAS keys", generateValidResponse (foldr (++) "" ("Found: " : ["13 keys (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt NAMEHAS 1", generateValidResponse (foldr (++) "" ("Found: " : ["6 house1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","18 bridge1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","35 person1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS chair", generateValidResponse (foldr (++) "" ("Found: " : ["4 chair1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","5 chair2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","34 chair3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 30", generateValidResponse (foldr (++) "" ("Found: " : [])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 1", generateValidResponse (foldr (++) "" ("Found: " : ["1 glasses (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 33", generateValidResponse (foldr (++) "" ("Found: " : ["33 closet (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 35", generateValidResponse (foldr (++) "" ("Found: " : ["35 person1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt NAMEHAS mirror", generateValidResponse (foldr (++) "" ("Found: " : ["32 mirror (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt NAMEHAS 3", generateValidResponse (foldr (++) "" ("Found: " : ["8 house3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11","37 person3 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/street.txt IDIS 36", generateValidResponse (foldr (++) "" ("Found: " : ["36 person2 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 25", generateValidResponse (foldr (++) "" ("Found: " : ["25 pen (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/bag.txt IDIS 14", generateValidResponse (foldr (++) "" ("Found: " : ["14 picture1 (1.0,2.0,3.0) (2.0,3.0,4.0) rectangular 11"])))
 ,("158.130.58.218 GET ./AssetServerStorage/room.txt IDIS 39", generateValidResponse (foldr (++) "" ("Found: " : [])))]
