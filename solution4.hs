import Data.List(transpose) 
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [_] = Just []
safeInit (x:xs) = let Just ys = safeInit xs in Just (x:ys)

safeInit' :: [a] -> Maybe [a]
safeInit' [] = Nothing
safeInit' xs = Just (init xs)

-- 出来上がりがどうなればいいのかよくわからなかった。
-- これは、splitWith isSpaceがwordsになるようにしたつもり

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs 
	| null first = splitWith p rest'
	| otherwise = first: splitWith p rest'
	where
		(first, rest) = span p xs
		rest' = drop 1 rest -- restの最初はpがfalseを返すような値が入ってるはず。 
		
headOfLine :: String -> IO ()
headOfLine str = putStrLn (map head (lines str))

transposeText :: String -> String
transposeText str = unlines (transpose  (lines str))

