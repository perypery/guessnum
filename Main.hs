module Main where

import System.Random ( getStdGen, StdGen)
import GuessGame ( guessMove, History)
import Data.Char ( digitToInt)

-- main
main :: IO ()
main = do
    ranGen <- getStdGen
    playWithAi ranGen [1, 4, 3, 2]  -- or playWithHuman



-- print a list as a 4-digit number
pretty :: [Int] -> String
pretty = concatMap show

-- response to a try
answer :: [Int] -> [Int] -> (Int, Int)
answer ans try = (appeared, inposition) where
    appeared    = length (filter (`elem` ans) try)
    inposition  = let bs = (zipWith (==) ans try) in length $ filter id bs


-- display history
displayHistory :: History -> IO ()
displayHistory = display' where
    display' h = do
        putStrLn ">>>"
        mapM_ (\(try, a, b) -> putStrLn $ "guess " ++ pretty try ++ " with " ++ show a ++ "A" ++ show b ++ "B") h
        return ()

-- play the game with ai oracle
playWithAi :: StdGen -> [Int] -> IO ()
playWithAi ranGen x = do
    let h = singleCase ranGen x 8; len = length h
    putStrLn $ show len ++ " steps to guess " ++ show x
    displayHistory h
    return ()

-- test a single case with AI
singleCase :: StdGen -> [Int] -> Int -> History
singleCase r0 ans n0 = proceed r0 n0 [] where
    proceed ranGen n h
        | n <= 0 = h      -- after 10 iterations
        | otherwise = -- trace (">> " ++ show n ++ ":" ++ pretty ans ++ show ans ++ show h ++ "\n") $
            let (try, ranGen') = guessMove h ranGen;
                (a,b) = answer ans try
                h' = h++[(try, a, b)]
            in
                if ans == try then h' else proceed ranGen' (n-1) h'

-- play the game with human oracle
playWithHuman :: IO ()
playWithHuman = do
    putStrLn "Pick any number in [1000 .. 9999] with NO duplicate digits. Press ENTER when you are ready..."
    _ <- getLine
    putStrLn "If you answer my questions honestly, I can know your number!"
    ranGen <- getStdGen
    guess [] ranGen
    putStrLn "Press ENTER to exit."
    _ <- readLn :: IO String
    return ()

-- the guess iteration
guess :: [([Int], Int, Int)] -> StdGen -> IO ()
guess history ranGen =
  let (try, ranGen') = guessMove history ranGen in
   do
     displayHistory history
     putStrLn $ "I guess it is: " ++ pretty try
     putStrLn "How many digits appeared in your number ?"
     a <- readLn :: IO Int
     putStrLn "How many digits appeared and are in the right position ?"
     b <- readLn :: IO Int
     guess ((try, a, b) : history) ranGen'
