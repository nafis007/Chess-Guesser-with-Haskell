-- File    : Project2.hs
-- Author  : Mohammad Nafis Ul Islam <LMS Login Id : mislam3>
-- Purpose : Implementing the guessing part of the ChessGuess Game.

{- | ChessGuess Game Description:

	In this game, there will be one hider and one guesser. The hider declares
	a size, which will be in range from 0 to 32. Then the hider selects any 
	number of chess-pieces up to that size, from a standard chess-set, as the 
	target set. The guesser can then repeatedly guess the target set by 
	selecting a subset of chess-pieces, from the standard chess-set, up to the 
	size declared by the hider. Upon each guess, the hider gives a feedback to 
	the guesser telling 3 things : 
	i)  how many of the guessed pieces were correct, 
	ii) how many of the guessed pieces had the right kind but the wrong colour, 
	iii)how many of the guessed pieces had the right colour but wrong kind.
	The guesser uses this feedback to guess again and again until he/she 
	guesses all the target pieces correctly. The objective of this game is for 
	the guesser to find the target set using the fewest possible guesses.

	File Summary :

	We have implemented the guessing part of the game in this single file. 
	This program will be tested with the hider part of the game, that is 
	implemented in the server, in the test file for this project.
	There are two major functions in this file : "initialGuess" and
	"nextGuess" along with many other necessary functions. 
	Their descriptions are given along with them.
-}

module Project2 (initialGuess, nextGuess, GameState) where

import Data.List

{- | The 'chessPieces' list contains all kinds of chess pieces of a standard
	 chess-set. Each chess-piece is represented as a two-character string.
	 The first character denotes its colour, 'B' for 'Black' and 'W' for
	 'White'. The second character denotes the kind of a chess piece,
	 'K' for 'King', 'Q' for 'Queen', 'N' for 'Knight', 'R' for 'Rook'
	 'B' for 'Bishop' and 'P' for 'Pawn'.
-}
chessPieces :: [String]
chessPieces = ["BB","WB","BR","WR","BP","WP","BN","WN","WQ","BQ","WK","BK"]				  

-- | This list contains those chess-pieces, that can occur in a standard
--   chess-set at most 1 time.
oneCountPieces = ["BK","WK","BQ","WQ"]

-- | This list contains those chess-pieces, that can occur in a standard
--   chess-set at most 2 times.
twoCountPieces = ["BB","WB","BR","WR","BN","WN"]

-- | This list contains those chess-pieces, that can occur in a standard
--   chess-set at most 8 times.
eightCountPieces = ["BP","WP"]	


-------------------------------------------------------------------------------
{- | The "getPieceCount" function takes a list of strings (chess-pieces list) 
     and a string (single chess-piece), and returns the number of occurrences 
	 of that string in that list.	 
-}
getPieceCount :: [String] -> String -> Int
getPieceCount [] _ = 0
getPieceCount (x:xs) y 
   | x == y = 1 + (getPieceCount xs y)
   | otherwise = getPieceCount xs y


-------------------------------------------------------------------------------
{- | The "maxAllowedCheck" function takes two string lists and an integer; and
     returns an array of boolean values. The boolean values show whether a 
	 piece from a candidateList has occured validly in the guess list. 
	 For example, to check that "BK" has not occured more than once. We 
	 give the candidateList in the second argument. We get a candidate 
	 from the candidateList and check its occurrence count in the guess list 
	 using the above "getPieceCount" function. Finally, we map it with a 
	 (<=maxCount) function to check whether their respective counts
	 are valid or not. If valid, it will give a True value, otherwise a False
	 value. That is how the boolean array gets generated for a specific 
	 candidateList and a guess list. 
	 Here, the first argument is the guess, the second argument is the 
	 candidateList and the third argument is the maxCount.
-}
maxAllowedCheck :: [String] -> [String] -> Int -> [Bool]
maxAllowedCheck [] _ _= [True]
maxAllowedCheck guess candidateList maxCount =
	map (\x -> getPieceCount guess x <= maxCount) (candidateList)

	
-------------------------------------------------------------------------------	
{- | The "checkValidGuess" function takes a string list and returns a boolean
     value. The string list is actually a guess with some chess-pieces in it 
	 and the boolean value denotes whether the guess is valid or not. 
	 For example, a guess ["WK","WK"] is not valid, because "WK" can occur
	 at most once in a chess-set. This is done easily with the 
	 "maxAllowedCheck" function above, by providing 3 different types of
	 candidateLists along with their maximum allowed count and the guess.
	 This large boolean array is then folded to a single value. If
	 there is a single false in the returned boolean arrays by the 
	 "maxAllowedCheck" function, this will not be a valid guess option.
-}
checkValidGuess :: [String] -> Bool
checkValidGuess guess = foldr (&&) True
    ((maxAllowedCheck guess oneCountPieces 1) ++
     (maxAllowedCheck guess twoCountPieces 2) ++
     (maxAllowedCheck guess eightCountPieces 8))


-------------------------------------------------------------------------------
{- | The "generateValidCombination" takes a list of string lists and keeps
	 only the valid string lists, and returns the filtered list of string 
	 lists. The first argument actually denotes the list of all possible 
	 guesses. After this filtering, the result gives us the list of only 
	 valid guesses with the help of the above "checkValidGuess" function.     	
-}
generateValidCombination :: [[String]] -> [[String]]
generateValidCombination [] = []
generateValidCombination (x:xs) = 
	if checkValidGuess x == True then x : (generateValidCombination xs)
	else generateValidCombination xs	 


-------------------------------------------------------------------------------	
{- | The "generateSubSet" function takes an integer list, and an integer count,
     and returns a list of subsets of that first list, where the subset size 
	 is equal to that given count. This follows the simple theory of generating
	 subsets. This function will be needed to generate the multisets next.
-}
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


-------------------------------------------------------------------------------	
-- | This "nonPositiveIntList" will be used to generate multisets. This list
--   just contains an infinite list of negative integers starting from 0.	   
nonPositiveIntList = [0,-1..]

{- | The function "integerMultiSets" takes two integers and returns a list of
     integer multisets. The first integer argument denotes the range of 
	 integers to be used, and the second integer argument denotes the size of
	 each multiset. The basic theory of multisets can be seen
	 from https://en.wikipedia.org/wiki/Multiset#Counting_multisets
-}
integerMultiSets :: Int -> Int -> [[Int]]	   
integerMultiSets n k = 
 map (\x -> zipWith (+) nonPositiveIntList x) (generateSubSet [1..lastElem] k)
    where lastElem = (n+k-1)


-------------------------------------------------------------------------------
{- | The "getNthElem" function returns the nth element from a string list.
     "n" is denoted by the first argument, the second argument is the 
	 string list.
-}
getNthElem :: Int -> [String]  -> String
getNthElem n (x:xs)
	 | n <= length (x:xs) = find 1 n (x:xs)
			 where
			   find m n (x:xs)
				 | m == n = x			
				 | m < n = find (m+1) n xs	


-------------------------------------------------------------------------------	
{- | The "convertIntoChessPiece" function takes an integer list and returns
     a string list (representing chess-pieces), converting the integers into
     its corresponding chess-piece representation. For example, integer 1 will 
	 be converted to the first element of "chessPieces" list, integer 5 will be 
	 converted to the 5th element of "chessPieces" list and so on.
-}							 
convertIntoChessPiece :: [Int] -> [String]
convertIntoChessPiece [] = []
convertIntoChessPiece (x:xs) = 
	(getNthElem x chessPieces) : (convertIntoChessPiece xs)


-------------------------------------------------------------------------------
{- | The "getCombinations" function takes two integers and returns
     a list of string lists, which is actually a list of chess-pieces lists.
	 This result will give all valid possible guesses for a given size.
	 Here, the size is the second argument and the first argument denotes
	 the range, to be fed into "integerMultiSets" function's first argument.
	 As from the explanation given above about "integerMultiSets" function,
	 we know that it will return a list of integer multisets. Since, we
	 need the multisets of chess-pieces (represented as strings), we
	 convert that integer multisets list, into a list of chess-pieces lists,
	 using the "convertIntoChessPiece" function. Finally, we filter the list
	 and keep the valid guesses only, by applying the function 
	 "generateValidCombination" ahead of it.
-} 
getCombinations :: Int -> Int -> [[String]]
getCombinations _ 0 = [[]]
getCombinations n size = 
 generateValidCombination (map convertIntoChessPiece (integerMultiSets n size)) 
 

-------------------------------------------------------------------------------	
{- | The "allSizeCombination" takes an integer (n) and returns a list of string 
     lists, which contains all valid possible guesses, starting from, 
	 for game size 0, to a game size denoted by the integer given in the 
	 first argument. 
	 This is the list of guesses (string lists denoting chess-pieces)
	 which will be changed according to the game state and used 
	 again and again until guessing correctly.
	 
-}		  
allSizeCombination :: Int -> [[String]]
allSizeCombination 0 = [[]]
allSizeCombination n = (getCombinations 12 n) ++ allSizeCombination (n-1)


-------------------------------------------------------------------------------
-- | Our GameState is a list of string lists, where each string denotes a
--   chess-piece.
type GameState = [[String]]


-------------------------------------------------------------------------------
{- | The "initialGuess" function just takes an integer, which denotes the
     game size and returns a tuple of first guess and first game state. 
	 The first guess will be the first element from the generated 
	 combinations for that game size, and the first game state will be
	 the rest of the combinations.
-}
initialGuess :: Int -> ([String],GameState)
initialGuess 0 = ([],[[]])
initialGuess n = (firstGuess, firstGameState)
 where firstGuess = head (allSizeCombination n)
       firstGameState = tail (allSizeCombination n)


-------------------------------------------------------------------------------
{- | The "generateNextGameState" function takes a tuple of current guess 
	 (a string list) and game state, and the feedback tuple for that guess. 
	 Then based on this information, it returns the next game state. 
	 To generate next game state, we just discard the inconsistent guesses from 
	 the current game state. To check consistency, we use the "response" 
	 function and check which guesses in the current game state provide the 
	 same response as the current guess. We keep them, and discard the 
	 inconsistent ones. Then we return this reformed game state as the 
	 next game state.
-}
generateNextGameState :: ([String],GameState) -> (Int,Int,Int) -> GameState
generateNextGameState ([],_) (_,_,_) = []       
generateNextGameState (_,[[]]) (_,_,_) = [[]]
generateNextGameState (x,(ys:yss)) (correct,kind,color) =
   if response x ys == (correct,kind,color) then 
	[ys] ++ (generateNextGameState (x,yss) (correct,kind,color))
   else 
	generateNextGameState (x, yss) (correct,kind,color)


------------------------------------------------------------------------------- 
{- | The "nextGuess" function takes a tuple of current guess (string list)
     and game state, and the feedback tuple for that guess. Then it returns a 
	 tuple containing the next guess (a string list denoting some chess-pieces) 
	 and the new game state. The next guess is the first element of the next
	 game state and the next game state is the rest of the next game state,
	 which was obtained by the help of the "generateNextGameState" function.
-}  
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (cG,cGS) (correct,kind,color) = (next_guess, nextGameState)
 where next_guess = head (generateNextGameState (cG,cGS) (correct,kind,color))
       nextGameState=tail (generateNextGameState (cG,cGS) (correct,kind,color))

	   
-------------------------------------------------------------------------------     
-- This function "response" was taken from the "Project2Test.hs" file 
-- and used in this project. The "Project2Test.hs" file was provided by 
-- the teaching staffs of COMP90048 Course, as part of the Project 2.
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

