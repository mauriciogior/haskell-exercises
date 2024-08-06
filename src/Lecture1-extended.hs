
module Lecture1Extended
    ( isEven
    , factorial
    , removeDuplicates
    ) where

import Data.Char ( toUpper )

-- Implement a function isEven :: Int -> Bool that returns True if a number is even, and False otherwise.
-- Test case 1: isEven 4 should return True
-- Test case 2: isEven 7 should return False

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- Write a function factorial :: Integer -> Integer that calculates the factorial of a given number using recursion.
-- Test case 1: factorial 5 should return 120
-- Test case 2: factorial 0 should return 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n
    | n < 0     = 0
    | otherwise = (*) n $ factorial $ n - 1


-- Create a function removeDuplicates :: Eq a => [a] -> [a] that removes duplicate elements from a list.
-- Test case 1: removeDuplicates [1,2,2,3,4,4,5] should return [1,2,3,4,5]
-- Test case 2: removeDuplicates "hello" should return "helo"

-- Original that I did
-- removeDuplicates :: Eq a => [a] -> [a]
-- removeDuplicates = go []
--     where
--         go :: Eq a => [a] -> [a] -> [a]
--         go un (x:xs)
--             | null xs && not (x `elem` un)     = (un ++ [x])
--             | null xs                          = un
--             | not (x `elem` un)                = go (un ++ [x]) xs
--             | otherwise                        = go un xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = reverse . go []
  where
    go acc [] = acc
    go acc (x:xs)
      | x `elem` acc = go acc xs
      | otherwise    = go (x:acc) xs

-- Implement myMap :: (a -> b) -> [a] -> [b] that mimics the behavior of the standard map function.
-- Test case 1: myMap (*2) [1,2,3,4] should return [2,4,6,8]
-- Test case 2: myMap toUpper "hello" should return "HELLO"

-- Original that I did
-- myMap :: (a -> b) -> [a] -> [b]
-- myMap = go []
--     where
--         go :: [b] -> (a -> b) -> [a] -> [b]
--         go newl _ [] = reverse newl
--         go newl fun (x:xs) = go ((fun x):newl) fun xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Write a function countVowels :: String -> Int that counts the number of vowels in a given string.
-- Test case 1: countVowels "hello world" should return 3
-- Test case 2: countVowels "rhythm" should return 0

countVowels :: String -> Int
countVowels = length . filter isVowel
    where
        isVowel :: Char -> Bool
        isVowel c = c `elem` ['a', 'e', 'i', 'o', 'u']

-- Create a function fibonacci :: Int -> Int that returns the nth Fibonacci number using recursion.
-- Test case 1: fibonacci 7 should return 13
-- Test case 2: fibonacci 0 should return 0

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci n = n + (n - 1)

-- Implement myFilter :: (a -> Bool) -> [a] -> [a] to mimic the behavior of the standard filter function.
-- Test case 1: myFilter even [1..10] should return [2,4,6,8,10]
-- Test case 2: myFilter (>5) [1,3,5,7,9] should return [7,9]

-- My original solution
-- myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter = go []
--     where
--         go :: [a] -> (a -> Bool) -> [a] -> [a]
--         go acc fun [] = reverse acc
--         go acc fun (x:xs) = go ((if (fun x) then [x] else []) ++ acc) fun xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise = myFilter p xs

-- Write a function isPalindrome :: String -> Bool that checks if a given string is a palindrome.
-- Test case 1: isPalindrome "racecar" should return True
-- Test case 2: isPalindrome "hello" should return False

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome str
    | length str == 1      = True
    | head str == last str = isPalindrome $ init $ tail str
    | otherwise            = False
    

-- Create a function rotateList :: Int -> [a] -> [a] that rotates a list by n positions to the left.
-- Test case 1: rotateList 2 [1,2,3,4,5] should return [3,4,5,1,2]
-- Test case 2: rotateList (-1) "hello" should return "ohell"

-- My solution
-- rotateList :: Int -> [a] -> [a]
-- rotateList 0 list = list
-- rotateList n (x:xs) = rotateList (n - 1) (xs ++ [x])

rotateList :: Int -> [a] -> [a]
rotateList n list =
    let len = length list
        normalizedN = n `mod` len
    in take len            -- it takes the original length of the list only [3,1,2]
        $ drop normalizedN -- it drops the rotation amount, let's say 2, gives us [3,1,2,3,1,2,3,...]
        $ cycle list       -- cycle creates an infinite repeated list like [1,2,3,1,2,3,1,2,3,...]


-- Implement a function zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] that behaves like the standard zipWith function.
-- Test case 1: zipWith' (+) [1,2,3] [4,5,6] should return [5,7,9]
-- Test case 2: zipWith' (++) ["a","b","c"] ["1","2","3"] should return ["a1","b2","c3"]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' fun l1 l2
    | length l1 /= length l2 = error "List lengths don't match"
    | otherwise = go [] fun l1 l2
    where
        go :: [c] -> (a -> b -> c) -> [a] -> [b] -> [c]
        go acc _ [] [] = reverse acc
        go acc fun (x:xs) (y:ys) = go ((fun x y):acc) fun xs ys

-- Implement a function customSort :: (a -> a -> Ordering) -> [a] -> [a] that sorts a list using a custom comparison function. Test cases:
-- customSort (\x y -> compare y x) [1,3,2,5,4] should return [5,4,3,2,1]
-- customSort (\x y -> compare (length x) (length y)) ["abc", "a", "ab"] should return ["a", "ab", "abc"]


listPairs :: Int -> [[a]] -> [a] -> [[a]]
listPairs _ pairs [] = reverse pairs
listPairs offset pairs list
    | offset > 0 = listPairs 0 ((take 1 list):pairs) (drop 1 list)
    | otherwise = listPairs offset ((take 2 list):pairs) (drop 2 list)
    
-- [4,1,5,6,9,2,3,0]
-- [4,1] [5,6] [9,2] [3,0]
-- [1,4] [5,6] [2,9] [0,3]
-- [1] [4,5] [6,2] [9,0] [3]
-- [1] [4,5] [2,6] [0,9] [3]
-- [1,4] [5,2] [6,0] [9,3]
-- [1,4] [2,5] [0,6] [3,9]
-- [1] [4,2] [5,0] [6,3] [9]
-- [1] [2,4] [0,5] [3,6] [9]
-- [1,2] [4,0] [5,3] [6,9]
-- [1,2] [0,4] [3,5] [6,9]
-- [1] [2,0] [4,3] [5,6] [9]
-- [1] [0,2] [3,4] [5,6] [9]
-- [1,0] [2,3] [4,5] [6,9]
-- [0,1] [2,3] [4,5] [6,9]
-- [0,1,2,3,4,5,6,9]

-- My solution
-- customSort :: (a -> a -> Ordering) -> [a] -> [a]
-- customSort = doSort True
--     where
--         doSort :: Bool -> (a -> a -> Ordering) -> [a] -> [a]
--         doSort r fn list
--             | isSorted fn list = list
--             | otherwise =
--                 let offset = if r then 0 else 1
--                     pairs = listPairs offset [] list
--                 in doSort (not r) fn $ concat $ map (sortPair fn) pairs

--         isSorted :: (a -> a -> Ordering) -> [a] -> Bool
--         isSorted fn []  = True
--         isSorted fn [a] = True
--         isSorted fn (x:xs)
--             | fn x (head xs) == GT = False
--             | otherwise = isSorted fn xs

--         listPairs :: Int -> [[a]] -> [a] -> [[a]]
--         listPairs _ pairs [] = reverse pairs
--         listPairs offset pairs list
--             | offset > 0 = listPairs 0 ((take 1 list):pairs) (drop 1 list)
--             | otherwise = listPairs offset ((take 2 list):pairs) (drop 2 list)

--         sortPair :: (a -> a -> Ordering) -> [a] -> [a]
--         sortPair fn [] = []
--         sortPair fn [a] = [a]
--         sortPair fn (x:xs) =
--             let right = head xs
--             in if fn x right == GT then [right, x] else (x:xs)

-- Another algorithm
-- customSort :: (a -> a -> Ordering) -> [a] -> [a]
-- customSort _ [] = []
-- customSort cmp (x:xs) =
--     let smaller = customSort cmp (filter (cmpb cmp x) xs)
--         bigger = customSort cmp (filter (not . cmpb cmp x) xs)
--     in smaller ++ [x] ++ bigger
--     where
--         cmpb :: (a -> a -> Ordering) -> a -> a -> Bool
--         cmpb cmp x y = cmp x y /= GT

-- Using list comprehension
customSort :: (a -> a -> Ordering) -> [a] -> [a]
customSort _ [] = []
customSort cmp (x:xs) =
    let smaller = customSort cmp (filter (\y -> cmp y x /= GT) xs)
        bigger  = customSort cmp (filter (\y -> cmp y x == GT) xs)
    in smaller ++ [x] ++ bigger

-- Write a function groupByKey :: Eq k => [(k, v)] -> [(k, [v])] that groups a list of key-value pairs by key. Test cases:
-- groupByKey [("a", 1), ("b", 2), ("a", 3), ("c", 4), ("b", 5)] should return [("a",[1,3]),("b",[2,5]),("c",[4])]
-- groupByKey [(1,"a"), (2,"b"), (1,"c"), (3,"d")] should return [(1,["a","c"]),(2,["b"]),(3,["d"])]

groupByKey :: Eq k => [(k, v)] -> [(k, [v])]
groupByKey [] = []
groupByKey list = map (\(k, vs) -> (k, reverse vs)) $ reverse $ go [] list
    where
        go :: Eq k => [(k, [v])] -> [(k, v)] -> [(k, [v])]
        go grouped [] = grouped
        go grouped ((k, v):kvs) =
            let has = any (\(l, _) -> l == k) grouped
            in if has
               then go (map (\(l, ms) -> if l == k then (l, v:ms) else (l, ms)) grouped) kvs
               else go ((k, [v]):grouped) kvs

-- Implement a function deepReverse :: [a] -> [a] that reverses a list and all nested lists within it. Test cases:
-- deepReverse [1, [2, 3], 4, [5, [6, 7]]] should return [[7, 6], 5], 4, [3, 2], 1]
-- deepReverse ["a", ["b", "c"], ["d", ["e", "f"]]] should return [["f", "e"], "d"], ["c", "b"], "a"]

data NestedList a = Elem a | List [NestedList a]

deepReverse :: NestedList -> NestedList
deepReverse 

-- Write a function findLongestPalindromeSubstring :: String -> String that finds the longest palindrome substring in a given string. Test cases:
-- findLongestPalindromeSubstring "babad" should return "bab" or "aba"
-- findLongestPalindromeSubstring "cbbd" should return "bb"

-- Implement a function balancedParentheses :: String -> Bool that checks if a string of parentheses is balanced. Test cases:
-- balancedParentheses "((()))()()" should return True
-- balancedParentheses "())(" should return False

-- Write a function mergeSort :: Ord a => [a] -> [a] that implements the merge sort algorithm. Test cases:
-- mergeSort [3,1,4,1,5,9,2,6,5,3,5] should return [1,1,2,3,3,4,5,5,5,6,9]
-- mergeSort ["banana", "apple", "cherry", "date"] should return ["apple", "banana", "cherry", "date"]

-- Implement a function runLengthEncode :: Eq a => [a] -> [(a, Int)] that performs run-length encoding on a list. Test cases:
-- runLengthEncode "AABBBCCCC" should return [('A',2),('B',3),('C',4)]
-- runLengthEncode [1,1,2,3,3,3,4,4,4,4] should return [(1,2),(2,1),(3,3),(4,4)]

-- Write a function permutations :: [a] -> [[a]] that generates all permutations of a list. Test cases:
-- permutations [1,2,3] should return [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- permutations "abc" should return ["abc","acb","bac","bca","cab","cba"]

-- Implement a function longestCommonSubsequence :: Eq a => [a] -> [a] -> [a] that finds the longest common subsequence of two lists. Test cases:
-- longestCommonSubsequence "ABCDGH" "AEDFHR" should return "ADH"
-- longestCommonSubsequence [1,2,3,4,1] [3,4,1,2,1,3] should return [3,4,1]

-- Write a function evaluateRPN :: String -> Double that evaluates a mathematical expression in Reverse Polish Notation. Test cases:
-- evaluateRPN "3 4 +" should return 7.0
-- evaluateRPN "5 1 2 + 4 * + 3 -" should return 14.0


-- Implement takeWhile' that takes elements from a list as long as they satisfy a predicate.

-- takeWhile' (<3) [1,2,3,4,1,2,3,4] should return [1,2]
-- takeWhile' isUpper "ABCdEF" should return "ABC"
-- Write dropWhile' that drops elements from a list as long as they satisfy a predicate.

-- dropWhile' (<3) [1,2,3,4,1,2,3,4] should return [3,4,1,2,3,4]
-- dropWhile' isUpper "ABCdEF" should return "dEF"
-- Implement zip3' that zips three lists together into a list of triples.

-- zip3' [1,2,3] [4,5,6] [7,8,9] should return [(1,4,7),(2,5,8),(3,6,9)]
-- zip3' [1,2] [3,4,5] [6,7,8,9] should return [(1,3,6),(2,4,7)]
-- Create splitAt' that splits a list at the given index.

-- splitAt' 3 [1,2,3,4,5] should return ([1,2,3],[4,5])
-- splitAt' 1 "hello" should return ("h","ello")
-- Implement wordsWhen that splits a list into sublists based on a predicate.

-- wordsWhen (==',') "apple,banana,cherry" should return ["apple","banana","cherry"]
-- wordsWhen (==' ') "hello world" should return ["hello","world"]
-- Write transposeMatrix that transposes a matrix (list of lists).

-- transposeMatrix [[1,2,3],[4,5,6]] should return [[1,4],[2,5],[3,6]]
-- transposeMatrix [[1,2],[3,4],[5,6]] should return [[1,3,5],[2,4,6]]
-- Implement mapAccumL' that maps a function over a list while passing an accumulating parameter.

-- mapAccumL' (\acc x -> (acc + x, acc)) 0 [1,2,3,4] should return (10,[0,1,3,6])
-- mapAccumL' (\acc x -> (acc ++ [x], length acc)) [] "hello" should return ("hello",[0,1,2,3,4])
-- Create findIndices' that finds all indices of elements satisfying a predicate.

-- findIndices' even [1,2,3,4,5,6] should return [1,3,5]
-- findIndices' (=='l') "hello" should return [2,3]
-- Implement groupBy' that groups adjacent elements in a list according to a given equality function.

-- groupBy' (\x y -> (x mod3) == (ymod 3)) [1,2,4,5,7,8,10,11] should return [[1],[2,4,5,7,8,10],[11]]
-- groupBy' (\x y -> x == y) "mississippi" should return ["m","i","ss","i","ss","i","pp","i"]
-- Write scanl1' that is similar to foldl1 but returns a list of successive reduced values from the left.

-- scanl1' (+) [1,2,3,4,5] should return [1,3,6,10,15]
-- scanl1' max [1,3,2,4,5] should return [1,3,3,4,5]