module Project2 (initialGuess, nextGuess, GameState) where

import Data.List

				  
				  
chessPieces :: [String]
chessPieces = ["BP","WP","BB","WB","BR","WR","BN","WN","BQ","WQ","BK","WK"]				  

oneCountElements = ["BK","WK","BQ","WQ"]
twoCountElements = ["BB","WB","BR","WR","BN","WN"]
eightCountElements = ["BP","WP"]	

generateSubSet :: [Int] -> Int -> [[Int]]
generateSubSet list 0 = [[]]

generateSubSet list elemCount =
	if length list < elemCount then []
	else firstPart ++ restPart where 
	 firstPart = generateSubSet (tail list) elemCount
	 restPart = map (x:) (generateSubSet y (elemCount-1)) 
	    where
	       x = head list
	       y = tail list
	   

nonPositiveIntList = [0,-1..]
	   
integerMultiSets n k = map (\x -> zipWith (+) nonPositiveIntList x) (generateSubSet [1..lastElem] k)
	where lastElem = (n+k-1)



convertIntoChessPiece :: [Int] -> [String]
convertIntoChessPiece [] = []

convertIntoChessPiece (x:xs) = (getNthElem x chessPieces) : (convertIntoChessPiece xs)
 

 
getCombinations :: Int -> Int -> [[String]]
getCombinations _ 0 = [[]]

getCombinations n size = generateUniqueCombination (map convertIntoChessPiece (integerMultiSets n size)) 
 

 
generateUniqueCombination :: [[String]] -> [[String]]
generateUniqueCombination [] = []

generateUniqueCombination (x:xs) = 
	if checkValidGuess x == True then x : (generateUniqueCombination xs)
	else generateUniqueCombination xs
 

 
getPieceCount :: [String] -> String -> Int
getPieceCount [] _ = 0

getPieceCount (x:xs) y 
   | x == y = 1 + (getPieceCount xs y)
   | otherwise = getPieceCount xs y



  
checkValidGuess :: [String] -> Bool
checkValidGuess guess = getCheckResult
    ((map (\x -> getPieceCount guess x <= 1) (oneCountElements)) ++
    (map (\x -> getPieceCount guess x <= 2) (twoCountElements)) ++
    (map (\x -> getPieceCount guess x <= 8) (eightCountElements)))

	
getCheckResult :: [Bool] -> Bool
getCheckResult [] = True

getCheckResult (x:xs) =
	if x == False then False
	else getCheckResult xs
 

 
getNthElem :: Int -> [String]  -> String
--getNthElem n [] = error "This is an empty list"
getNthElem n (x:xs)
     -- | n > length (x:xs) = error "The index is greater than the list size"
	 | n <= length (x:xs) = find 1 n (x:xs)
							 where
							   find m n (x:xs)
								 | m == n = x
								 | m < n = find (m+1) n xs			  
				  

allSizeCombination :: Int -> [[String]]
allSizeCombination 0 = [[]]

allSizeCombination n = (getCombinations 12 n) ++ allSizeCombination (n-1)



type GameState = [[String]]

initialGuess :: Int -> ([String],GameState)
initialGuess 0 = ([],[[]])
initialGuess n = (firstGuess, firstGameState)
 where firstGuess = head (allSizeCombination n)
       firstGameState = tail (allSizeCombination n)

	   

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)

--nextGuess ([],_) (_,_,_) = ([],[[]])
--nextGuess (_,[[]]) (_,_,_) = ([],[[]])
nextGuess (cS,cGS) (correct,kind,color) = (nextGuessdString, nextGameState)
 where nextGuessdString = head (generateNextGameState (cS,cGS) (correct,kind,color))
       nextGameState = tail (generateNextGameState (cS,cGS) (correct,kind,color))
	   

generateNextGameState :: ([String],GameState) -> (Int,Int,Int) -> GameState
generateNextGameState ([],_) (_,_,_) = [[]]
generateNextGameState (_,[[]]) (_,_,_) = [[]]

generateNextGameState (x,(ys:yss)) (correct,kind,color) =
   if response x ys == (correct,kind,color) then [ys] ++ (generateNextGameState (x,yss) (correct,kind,color))
   else generateNextGameState (x, yss) (correct,kind,color)
      

-- This function "response" was taken from the Project2Test.hs file and used here. This was provided by the
-- teaching staffs of COMP90048 Course, as part of the Project 2.
-- | Compute the correct answer to a guess.  First argument is the 
--   target, second is the guess.
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightKind, rightColor)
  where 
        common      = mintersect guess target
        right       = length common
        rguess      = foldr (delete) guess common
        rtarget     = foldr (delete) target common
        rightColor  = length $ mintersect (map (!!0) rguess) (map (!!0) rtarget)
        rightKind   = length $ mintersect (map (!!1) rguess) (map (!!1) rtarget)

mintersect :: Eq t => [t] -> [t] -> [t]
mintersect [] _ = []
mintersect (x:xs) r = if elem x r then x : mintersect xs (delete x r)
                      else mintersect xs r 

