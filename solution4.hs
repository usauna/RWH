import Data.List(transpose) 
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- 出来上がりがどうなればいいのかよくわからなかった。
-- これは、splitWith isSpace がwordsになるようにしたつもり

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs 
	| null first = splitWith p rest'
	| otherwise = first: splitWith p rest'
	where
		(first, rest) = break p xs
		rest' = drop 1 rest -- restの最初はpがfalseを返すような値が入ってるはず。 
		
headOfLine :: String -> IO ()
headOfLine str = putStrLn (map head (lines str))

transposeText :: String -> String
transposeText str = unlines (transpose  (lines str)) -- transpose はData.Listの関数。


-- P.97
-- Problem 1


asInt :: String -> Int
asInt ws = foldl (\acc c -> 10*acc + digitToInt c) 0 ws

-- Problem 2
asInt' :: String -> Int
asInt' ('-':ws) = negate $ asInt' ws
asInt' ws = foldl (\acc c -> 10*acc + digitToInt c) 0 ws

-- Problem 3

asInt'' :: String -> Int
asInt'' "" = error "empty"
asInt'' "-" = error "only \"-\""
asInt'' ('-':ws) = negate ( asInt'' ws)
asInt'' ws 
    | any (not.isDigit) ws = error "The argument contains non-digit character"
    | otherwise = foldl (\acc c -> if acc > (maxBound `div` 10 )
                                  then error "too large"
                                  else 10*acc + digitToInt c) 0 ws

-- Problem 4

type ErrorMessage = String

asIntEither :: String -> Either ErrorMessage Int
asIntEither "" = Left "empty"
asIntEither "-" = Left "only \"-\""
asIntEither ('-':ws) = case asIntEither ws of
                            Right n -> Right (negate n)
                            Left message  -> Left message
asIntEither ws = helper ws 0
    where
        helper [] acc = Right acc
        helper (c:cs) acc 
            | not (isDigit c) = Left ("non-digit " ++ [c])
            | acc > maxBound `div` 10 = Left "too big"
            | otherwise = helper cs (10*acc + digitToInt c) 

-- Problem 5, 6
concatWithFold :: [[a]] -> [a]
concatWithFold = foldr (++) [] 

-- Problem 7
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs) 
	| p x = x:myTakeWhile p xs
	| otherwise = []
	
takeWhileWithFold :: (a -> Bool) -> [a] -> [a]
takeWhileWithFold p xs = foldr (\x acc -> if p x then (x:acc) else []) [] xs


-- Problem 8, 9
-- I don't think group can be implemented well with folds though...
-- Actually Data.List module does not define groupBy with folds. 
-- It uses explicit recursion.
groupByWithFold :: (a -> a -> Bool) -> [a] -> [[a]]
groupByWithFold eq xs = case foldr helper ([], []) xs of
                             ([], acc) -> acc
                             (list1, list2) -> list1:list2
    where
        helper x ([], acc) = ([x], acc)
        helper x (tmp@(p:_), acc) 
           | x `eq` p = (x:tmp, acc)
           | otherwise = ([x], tmp:acc)

-- Problem 10
anyWithFold :: (a -> Bool) -> [a] -> Bool
anyWithFold p = foldr (\x acc -> p x || acc) False



unlinesWithFold :: [String] -> String
unlinesWithFold = foldr (\line acc -> line++('\n':acc)) [] 

-- foldr is good when we need to preserve the order
-- folds are not good at checking the relationship between elements of lists.
-- that's  why we cannot use folds to make group, words, ...etc
