module GuessGame where

import System.Random (randomR, StdGen)
import Data.Map

type Step = ([Int], Int, Int)   -- (guess, result)
type History = [Step]

-- make a decision upon the history
-- the returned generator should be different from the input one if the latter is used
-- if no possible candidate, just raise an error, actually, we should return a Maybe [Int], but that depends on the progress of lecture.
-- if the guessMove is implemented correctly, there should always be a candidate once this function is invoked
myfst (a, b, c) = a
myscd (a, b, c) = b
mythd (a, b, c) = c

mycalB :: [Int] -> [Int] -> Int -> Int
mycalB [] [] a = a
mycalB (x:xs) (y:ys) a
    | x == y = mycalB xs ys $ a + 1
    | otherwise = mycalB xs ys a
mycalB _ _ a = a

mycalA :: [Int] -> [Int] -> Int -> Int--first is X,second is mine
mycalA _ [] a = a
mycalA b (x:xs) a = mycalA b xs $ a + if(x `elem` b) then 1 else 0

mycal :: [Int] -> [Int] -> (Int, Int) -- first is their num, second is mine
mycal a b = (mycalA a b 0, mycalB a b 0)

myObey :: Step -> [Int] -> Bool
myObey (x, a, b) y = let p = mycal x y in fst(p) == a && snd(p) == b

myDelete :: Step -> [[Int]] -> [[Int]] -> [[Int]]--secend is new space, third is old space
myDelete _ n [] = n
myDelete step newspace (ox:oxs) = myDelete step ((if (myObey step ox) then [ox] else []) ++ newspace) oxs -- n:[[Int]], oxs::[[Int]],ox:[Int]

noRepeat :: [Int] -> Bool
noRepeat [a, b, c, d]
    | a == b = False
    | a == c = False
    | a == d = False
    | b == c = False
    | b == d = False
    | c == d = False
    | otherwise = True

getSpace :: History -> [[Int]]
getSpace [] = Prelude.filter noRepeat $ Prelude.map (\x -> [mod (div x 1000) 10, mod (div x 100) 10, mod (div x 10) 10, mod x 10]) [1000..9999]
getSpace (x:xs) = myDelete x [] $ getSpace xs

getScore :: Map (Int, Int) Int -> Int
getScore b = fold max 0 b

myupdate :: Int -> Maybe Int
myupdate x = Just (x + 1)

myScore :: [Int] -> [[Int]] -> (Map (Int, Int) Int) -> Int -- first is myinput, second is now space, third is record, fourth is last score
myScore myinput [] mymap = getScore mymap
myScore myinput (x:xs) mymap =
    let calResult = mycal x myinput--(Int,Int)
        inmap = member calResult mymap
    in if inmap then (myScore myinput xs $ update myupdate calResult mymap) else (myScore myinput xs $ insert calResult 1 mymap)
--fromList [((1, 1),1)]

myiteration :: ([Int],[[Int]], Int) -> [Int] -> ([Int],[[Int]], Int)
myiteration (old, space, oldvalue) new =
    let newvalue = myScore new space $ fromList []
    in if newvalue >= oldvalue then (old, space, oldvalue) else (new, space, newvalue)

myChoose :: [[Int]] -> [Int] -- first is for enumerate and space, third is result
myChoose a =
    let myfirst = ([],a,10000)
    in myfst $ Prelude.foldl myiteration myfirst a -- ([Int],a,value)

guessMove :: History -> StdGen -> ([Int], StdGen)
guessMove [] ranGen = ([9, 8, 7, 6], ranGen') where (r, ranGen') = randomR (1::Int, 4) ranGen
guessMove history ranGen =
    let (r, ranGen') = randomR (1::Int, 4) ranGen
        space = getSpace history -- space is [[Int]]
        result = myChoose space
    in (result, ranGen')


-- [([1, 2, 3, 4], 1, 0)]
--
