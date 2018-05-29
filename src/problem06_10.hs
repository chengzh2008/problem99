import Data.List (foldl')
import Control.Monad (liftM2)
import Control.Applicative ((<*>))

-- problem 6
-- Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome1 :: Eq a => [a] -> Bool
isPalindrome1 ls = ls == reverse ls

isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 [] = True
isPalindrome2 (x:[]) = True
isPalindrome2 (x:xs) = x == last xs && isPalindrome2 (init xs)

-- use foldr
isPalindrome3 :: Eq a => [a] -> Bool
isPalindrome3 ls = foldr (\(a1, a2) acc -> acc && a1 == a2) True $ zipped
  where
    sl = reverse ls
    zipped = zip ls sl

-- use Foldable
isPalindrome4 :: Eq a => [a] -> Bool
isPalindrome4 ls = and zipped
  where
    sl = reverse ls
    zipped = zipWith (==) ls sl

-- use liftM2
isPalindrome5 :: Eq a => [a] -> Bool
isPalindrome5 = liftM2 (==) id reverse -- need some thinking

-- use <*>
isPalindrome6 :: Eq a => [a] -> Bool
isPalindrome6 = (==) <*> reverse -- need some thinking

-- use Arrow &&&
-- TODO:

-- problem 7
{-
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:

* (my-flatten '(a (b (c d) e)))
(A B C D E)
Example in Haskell:

We have to define a new data type, because lists in Haskell are homogeneous.
-}

data NestedList a = Elem a | List [NestedList a]

flatten1 :: NestedList a -> [a]
flatten1 (Elem a) = [a]
flatten1 (List nestedList) = concat . fmap flatten1 $ nestedList

flatten2 :: NestedList a -> [a]
flatten2 (Elem a) = [a]
flatten2 (List nestedList) = concatMap flatten2 $ nestedList

flatten3 :: NestedList a -> [a]
flatten3 (Elem a) = [a]
flatten3 (List []) = []
flatten3 (List (x:xs)) = flatten3 x ++ flatten3 (List xs)

flatten4 :: NestedList a -> [a]
flatten4 (Elem a) = return a
flatten4 (List xs) = xs >>= flatten4 -- TODO: monadic form

flatten5 :: NestedList a -> [a]
flatten5 (Elem a) = return a
flatten5 (List xs) = foldMap flatten5 xs

-- foldr
flatten6 :: NestedList a -> [a]
flatten6 (Elem a) = return a
flatten6 (List xs) = foldr (++) [] $ map flatten6 xs
