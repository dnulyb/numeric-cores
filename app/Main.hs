module Main where

import Data.List(permutations)
import Control.Monad(foldM)
import Data.Maybe(isJust, fromJust, isNothing, fromMaybe)
import Data.Char(toLower, ord, chr)

main :: IO ()
main = do
    print $ core $ toList4 5 2 4 6        -- should be: Just 1
    print $ core $ toList4 1000 400 20 5  -- should be: Just 45
    print $ core $ toList4 1 2 3 4        -- should be: Nothing
    print $ coreStr "ebdf"                -- should be: Just 'a'
    print $ coreStr "utah"                -- should be: Just 'h'
    print $ coreStr "hand"                -- should be: Just 'b'

toList4 :: Double -> Double -> Double -> Double -> [Double]
toList4 a b c d = [a,b,c,d]

safeSub :: Double -> Double -> Maybe Double
safeSub x y
    | res >= 0 = Just res
    | otherwise = Nothing
    where res = x-y

safeMult :: Double -> Double -> Maybe Double
safeMult x y
    | res >= 0 = Just res
    | otherwise = Nothing
    where res = x*y

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x / y

funcs :: [Double -> Double -> Maybe Double]
funcs = [safeSub, safeMult, safeDiv]

core :: [Double] -> Maybe Int
core numbers
    | null res = Nothing
    -- Core is valid only if the result is an integer after the complete calculation
    | otherwise = minimum $ map (round <$>) $ filter isInteger res
    where res = filter isJust [eval numbers p | p <- permutations funcs]
          eval :: [Double] -> [Double -> Double -> Maybe Double] -> Maybe Double
          eval (n:ns) fs = foldM (\acc (f, x) -> acc `f` x) n (zip fs ns)
          eval _ _ = Nothing
          isInteger :: Maybe Double -> Bool
          isInteger (Just n) = n == fromInteger (round n)
          isInteger Nothing = False

-- String length must be exactly 4
coreStr :: String -> Maybe Char
coreStr [a,b,c,d]
    | isNothing res = Nothing
    | (fromJust res < 1) || (fromJust res > 26) = Nothing   -- Out of bounds for alphabet
    | otherwise = Just $ chr $ (+) 96 $ fromJust res
    where convert :: [Char] -> [Double]
          convert = map (subtract 96 . fromIntegral . ord . toLower)
          res = core $ convert [a,b,c,d]
coreStr _ = Nothing

-- Calculate cores for a file containing one word per line
wordsToCores :: String -> IO ()
wordsToCores filePath = do
    content <- readFile filePath
    putStr $ concatMap ((:"\n") . fromMaybe '-' . coreStr) $ lines content