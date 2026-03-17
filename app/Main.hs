module Main where

import Data.List(permutations)
import Control.Monad(foldM)
import Data.Maybe(isJust)

main :: IO ()
main = do
    print $ core 5 2 4 6        -- should be Just 1
    print $ core 1000 400 20 5  -- should be Just 150
    print $ core 1 2 3 4        -- should be Nothing

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

core :: Int -> Int -> Int -> Int -> Maybe Int
core a b c d
    | null res = Nothing
    | otherwise = minimum res
    where res = filter isJust [eval [a,b,c,d] p | p <- permutations funcs]
          eval :: [Int] -> [Int -> Int -> Maybe Int] -> Maybe Int
          eval (n:ns) fs = foldM (\acc (f, x) -> acc `f` x) n (zip fs ns)
          eval _ _ = Nothing

--coreStr :: String -> Char