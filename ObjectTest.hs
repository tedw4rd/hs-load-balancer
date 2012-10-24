-- Asset Server
-- by Ted Aronson and Anna Filatova

-- This module tests the functions of ObjectHandler.hs.

module ObjectTest where

import ObjectHandler

test1 = let ws1 = "2 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj1 = words ws1
	    rat1 = listToObject obj1
	    ws2 = "3 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj2 = words ws2
	    rat2 = listToObject obj2
	    ws3 = "4 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj3 = words ws3
	    rat3 = listToObject obj3
	    ws4 = "5 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj4 = words ws4
	    rat4 = listToObject obj4
	    ws5 = "6 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj5 = words ws5
	    rat5 = listToObject obj5
	    ws6 = "7 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj6 = words ws6
	    rat6 = listToObject obj6
	    ws7 = "8 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj7 = words ws7
	    rat7 = listToObject obj7
	    ws8 = "9 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj8 = words ws8
	    rat8 = listToObject obj8
	    
	in do
	    addObject "./RatTestFile.txt" rat1
	    addObject "./RatTestFile.txt" rat2
	    addObject "./RatTestFile.txt" rat3
	    addObject "./RatTestFile.txt" rat4
	    addObject "./RatTestFile.txt" rat5
	    addObject "./RatTestFile.txt" rat6
	    addObject "./RatTestFile.txt" rat7
	    addObject "./RatTestFile.txt" rat8


test3 = do rat <- (findObject "./RatTestFile.txt" (IdIs 2))
	   putStrLn (show (head rat))

test4 = do rat <- (findObject "./RatTestFile.txt" (IdIs 2))
	   if (rat == [(2, "Rat", (1,2,3), (4,5,6), "RatShape", 1337)])
		then putStrLn "Correct Rat Found"
		else putStrLn "Test 4 Did Not Find Correct Rat"

test5 = let ws1 = "2 Rat (1,2,3) (4,5,6) RatShape 1337"
	    obj1 = words ws1
	    rat1 = listToObject obj1
	    ws2 = "3 Cat (1,2,3) (4,5,6) CatShape 1337"
	    obj2 = words ws2
	    rat2 = listToObject obj2
	    ws3 = "4 Bat (1,2,3) (4,5,6) BatShape 1337"
	    obj3 = words ws3
	    rat3 = listToObject obj3
	    ws4 = "5 Mat (1,2,3) (4,5,6) MatShape 1337"
	    obj4 = words ws4
	    rat4 = listToObject obj4
	in do
	    addObject "./MultiRatTestFile.txt" rat1
	    addObject "./MultiRatTestFile.txt" rat2
	    addObject "./MultiRatTestFile.txt" rat3
	    addObject "./MultiRatTestFile.txt" rat4
	    mat <- (findObject "./MultiRatTestFile.txt" (NameHas "Mat"))
	    if (length mat == 1)
		then putStrLn "Found Mat"
	    	else putStrLn ("Test 5 Did Not Find Mat, Found " ++ show (length mat) ++ " Objects")

test6 = do rats <- findObject "./RatTestFile.txt" (NameHas "Rat")
	   if (length rats == 8)
		then putStrLn "Test 6 Got 8 Rats"
		else putStrLn ("Test 6 Did Not Get 8 Rats, Instead Found " ++ show (length rats))

test7 = do items <- findObject "./MultiRatTestFile.txt" All
	   if (length items == 4)
		then putStrLn "Test 7 Got 4 Items"
		else putStrLn ("Test 7 Did Not Get 4 Items, Instead Found " ++ show (length items))

main = do 
	test1
	test3
	test4
	test5
	test6
	test7