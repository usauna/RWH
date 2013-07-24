
-- This is the example of implementation of foldr using foldl.


myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f acc xs = foldl step id xs acc 
    where
        step g x a = g (f x a)
