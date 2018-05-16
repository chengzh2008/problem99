import Data.List (foldl')
-- problem 1
-- find the last element of a list
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (const id)

myLast'' :: [a] -> a
myLast'' = foldl1 (flip const)

myLast''' = head . reverse

myLast'''' :: [a] -> a
myLast'''' = foldl1 (curry snd)

myLast''''' :: [a] -> a
myLast''''' = foldr1 (curry snd)

myLast'''''' [] = error "empty list"
myLast'''''' xs = xs !! (length xs - 1)


-- problem 2
-- find the last but one element of a list
myButLast :: [a] -> a
myButLast [] = error "not enough items"
myButLast [x] = error "not enough items"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs

myButLast' xs = case reverse xs of
                  [] -> error "empty"
                  [x] -> error "not enough"
                  (x:y:xs) -> y

myButLast'' [x, _] = x
myButLast'' (_: xs) = myButLast'' xs

myButLast''' = head . reverse . init

myButLast'''' =  (!!1) . reverse

lastBut1 :: Foldable f => f a -> a
lastBut1 = fst . foldl (\(a, b) x -> (b, x)) (err1, err2)
  where err1 = error "empty list"
        err2 = error "only one item in list"

lastBut2 :: Foldable f => f a -> Maybe a
lastBut2 = fst . foldl (\(a, b) x -> (b, Just x)) (Nothing, Nothing)

-- problem 3
-- find the kth element of a list.
elementAt :: [a] -> Int -> a
elementAt xs n
  | n < 1 = error "n needs to be larger than 0"
  | n == 1 = head xs
  | otherwise = elementAt (tail xs) (n - 1)

elementAt' :: [a] -> Int -> a
elementAt' xs n
  | length xs < n = error "not enough item"
  | otherwise = last $ take n xs


elementAt'' :: [a] -> Int -> a
elementAt'' xs n
  | length xs < n = error "not enough item"
  | otherwise = head $ drop (n - 1) xs


-- problem 4
-- find the number of elements in the list

myLength :: [a] -> Int
myLength xs = go xs
  where go [] = 0
        go (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldr (\_ b -> b + 1) 0

myLength'' :: [a] -> Int
myLength'' = foldl (\b _ -> b + 1) 0

-- both above are very inefficient
myLength''' :: [a] -> Int
myLength''' = foldl' (\b _ -> b + 1) 0

myLength1 ls = snd $ last $ zip ls [1..]
myLength2 = fst . last . zip [1..] -- point-free
myLength3 = snd . last . flip zip [1..] -- point-free

myLength4 = sum . map (const 1) -- stack over flow with length 100000000

-- problem 5
-- reverse a list
reverse1 :: [a] -> [a]
reverse1 = reverse

reverse2 :: [a] -> [a]
reverse2 = foldr (\a b -> a:b) []

reverse3 :: [a] -> [a] -- slower than reverse2
reverse3 ls = go ls
  where
    go :: [a] -> [a]
    go [] = []
    go (x:xs) = go xs ++ [x]

reverse4 :: [a] -> [a]
reverse4 = foldl (\b a -> a:b) []
