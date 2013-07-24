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
