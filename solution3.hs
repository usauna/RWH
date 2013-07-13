-- RWH第三章　解答

import Data.List (sortBy, minimumBy)
import Data.Function (on)


-- Control.Applicativeは、おまけ。
import Control.Applicative((<*>))

-- Problem 1
-- Problem 2
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = sum (map (\_-> 1) xs)

-- Problem 3

-- length :: [a] -> Int
-- (/) :: (Fractional a) -> a -> a -> a
-- Int はFractionalクラスのインスタンスでは無いので、
-- fromIntegral :: (Integral a, Num b) => a -> b
-- を使わなくてはいけない。
mean :: (Floating a) => [a] -> a
mean xs = (sum xs) / (fromIntegral (length xs))


-- Problem 4

--よくあるやつ
-- reverseはPreludeで定義されている。
palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs



-- 我ながら綺麗な再帰。
-- (:) の優先順位は低めなので、この書き方で問題ない。
-- ちなみに、head x:xs = xという書き方はできないので注意。
palindrome' :: [a] -> [a]
palindrome' xs = helper xs []
    where
        helper [] acc = acc
        helper (x:xs) acc = x:helper xs (x:acc)


-- おまけ
-- applicative。
-- 関数を文脈と値として見る。
-- reverseは反転されたあとのリスト。
-- (++) は1つリストを受け取り、何かをくっつけてあげる関数と見る。
palindrome'' :: [a] -> [a]
palindrome'' = (++) <*> reverse


--　おまけ
--　アプリカティブと同様、文脈と値の集まりとして各々を見る。
palindrome''' :: [a] -> [a]
palindrome''' = reverse >>= flip (++)



-- Problem 5

--一番シンプルなやつ
-- (==)を使っているので、Eq クラス制約が必要。
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

--　haskell 99 からのパクリ
-- これは紙に書きだしてみる価値あり。
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = helper [] xs xs
    where
        helper list1 [] list2 = list1 == list2
        helper list1 [_] list2 = list1 == list2
        helper list1 (_:_:xs) (l:list2) = helper (l:list1) xs list2


-- アプリカティブ！
isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome''  = (==) <*> reverse

-- モナドモナド
isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = reverse >>= (==)


-- Problem 6
-- ラムダ式なし
-- compare :: (Ord a) => a -> a -> Ordering
-- data Ordering = LT | EQ | GT
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy compareLength
    where
        compareLength xs ys = compare (length xs) (length ys)


-- ラムダ式を作ったシンプルな解法
sortByLength' :: [[a]] -> [[a]]
sortByLength' = sortBy (\xs ys -> length xs `compare` length ys)


-- Data.Function の　onを使用。
-- compare `on` tail, compare `on` snd .. など
-- compare `on` 関数　
-- の形は、非常に便利。
-- 余談だが、haskellの標準ソートはマージソート。
sortByLength'' :: [[a]] -> [[a]]
sortByLength'' = sortBy (compare `on` length)


-- Problem 7

-- 普通の再帰
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [ys] = ys
intersperse x (ys:yss) = ys ++ (x:intersperse x yss)

-- concatMapを使った。
-- concatMap :: (a -> [b]) -> [a] -> [b]
intersperse2 :: a -> [[a]] -> [a]
intersperse2 x = tail . concatMap (x:)


-- foldr1と関数合成。
-- 割とお気に入り。
-- 空リストは、エラー
intersperse3 :: a -> [[a]] -> [a]
intersperse3 x = foldr1 (\xs -> (xs++) . (x:))


-- Problem 8

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

--　再帰
height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1+max (height left) (height right)

-- Problem 9

-- toLeftがbで左に曲がる
-- toRightがbで右に曲がる。
-- Straightはそのまま。

data Direction = ToLeft | Straight | ToRight
    deriving (Show, Eq)

type Point = (Double, Double)

-- Problem 10
-- Graham scanをウィキる。（英語版）
findDirection :: Point -> Point -> Point -> Direction
findDirection (xa,ya) (xb,yb) (xc,yc)
    | crossProduct < 0 = ToRight
    | crossProduct == 0 = Straight
    | crossProduct > 0 = ToLeft
        where
            crossProduct = (xb-xa)*(yc-ya) - (yb-ya)*(xc-xa)

-- Problem 11
-- 関数名がダサい
-- zipWith3の綺麗な活用例だと我ながら思う。
-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
--関数合成なし
eachDirection :: [Point] -> [Direction]
eachDirection pointList = zipWith3 findDirection pointList (tail pointList) (tail (tail  pointList))

-- 関数合成あり
-- でもあんまり変わってないｗ
eachDirection' :: [Point] -> [Direction]
eachDirection' pointList = zipWith3 findDirection pointList  (tail pointList)  (tail .tail $ pointList)



-- Problem 12
-- もう少し綺麗にかけるはず。

-- 関数合成なしバージョン
construct :: [Point] -> [Point]
construct listOfPoints  =   scan (startingPoint:sortByAngle listOfPoints)
    where
        startingPoint@(a, b) = minimumBy (compare `on` (\(x, y) -> (y, x))) listOfPoints
        sortByAngle xs = reverse (sortBy (compare `on`findAngle) xs)--cosが大きい方から先に来なくてはいけないので。
        findAngle (x, y)
            | (x,y)==(a,b) = -2 -- 最後に開始点に戻ってくるので、必ず最後に来るように設定。
            | otherwise = (x-a) / sqrt ((x-a)^2 + (y-b)^2) -- (x,y)、開始点とx軸がなす角のcos
        scan (x:y:z:rest)
            | findDirection x y z == ToRight = scan (x:z:rest)
            | otherwise = x:scan (y:z:rest) -- もし右に向かっていないならば、次をチェック。
        scan points = points -- catchallパターン。点の数が2つ以下ならば値をそのまま返す。

-- 関数合成あり。
-- そこまでかわってないｗ　

construct' :: [Point] -> [Point]
construct' listOfPoints  =  scan $ startingPoint:sortByAngle listOfPoints
    where
        startingPoint@(a, b) = minimumBy (compare `on` (\(x, y) -> (y, x))) listOfPoints
        sortByAngle = reverse . sortBy (compare `on`findAngle)
        findAngle (x, y)
            | (x,y)==(a,b) = -2 -- 最後に開始点に戻ってくるので、必ず最後に来るように設定。
            | otherwise = (x-a) / sqrt ((x-a)^2 + (y-b)^2) -- (x,y)、開始点とx軸がなす角のcos
        scan (x:y:z:rest)
            | findDirection x y z == ToRight = scan (x:z:rest)
            | otherwise = x:scan (y:z:rest) -- もし右に向かっていないならば、次をチェック。
        scan points = points -- catchallパターン。点の数が2つ以下ならば値をそのまま返す。

