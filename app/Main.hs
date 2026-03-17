module Main where

import Data.List(permutations)
import Control.Monad(foldM)
import Data.Maybe(isJust, fromJust, isNothing)
import Data.Char(toLower, ord, chr)

main :: IO ()
main = do
    print $ core $ toList4 5 2 4 6        -- should be: Just 1
    print $ core $ toList4 1000 400 20 5  -- should be: Just 150
    print $ core $ toList4 1 2 3 4        -- should be: Nothing
    print $ coreStr "ebdf"                -- should be: Just 'a'
    print $ coreStr "utah"                -- should be: Just 'h'

toList4 :: Int -> Int -> Int -> Int -> [Int]
toList4 a b c d = [a,b,c,d]

safeSub :: Int -> Int -> Maybe Int
safeSub x y
    | res >= 0 = Just res
    | otherwise = Nothing
    where res = x-y

safeMult :: Int -> Int -> Maybe Int
safeMult x y
    | res >= 0 = Just res
    | otherwise = Nothing
    where res = x*y

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y
    | isInteger res = Just $ round res
    | otherwise = Nothing
    where res = fromIntegral x / fromIntegral y
          isInteger :: Double -> Bool
          isInteger n = n == fromInteger (round n)


funcs :: [Int -> Int -> Maybe Int]
funcs = [safeSub, safeMult, safeDiv]

core :: [Int] -> Maybe Int
core numbers
    | null res = Nothing
    | otherwise = minimum res
    where res = filter isJust [eval numbers p | p <- permutations funcs]
          eval :: [Int] -> [Int -> Int -> Maybe Int] -> Maybe Int
          eval (n:ns) fs = foldM (\acc (f, x) -> acc `f` x) n (zip fs ns)
          eval _ _ = Nothing

-- String length must be exactly 4
coreStr :: String -> Maybe Char
coreStr [a,b,c,d]
    | isNothing res = Nothing
    | (fromJust res < 1) || (fromJust res > 26) = Nothing   -- Out of bounds for alphabet
    | otherwise = Just $ chr $ (+) 96 $ fromJust res
    where convert :: [Char] -> [Int]
          convert = map (subtract 96 . ord . toLower)
          res = core $ convert [a,b,c,d]
coreStr _ = Nothing