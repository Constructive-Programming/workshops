{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Data.List
import Data.Functor.Foldable

combinations :: [a] -> [[a]]
combinations elems = concat [ combs i elems | i <- [ 1 .. length elems ] ]
  where
  combs 0 _ = [[]]
  combs _ [] = [[]]
  combs k (x : xs) = map (x :) (combs (k - 1) xs) ++ combs k xs


combinations elems = unfoldr combs (length elems, []) elems where
  combs 0 xs = Nothing
  combs _ [] = Just ([], (0, []))
  combs k (x : xs) = Just (inner (k - 1) x xs, (k, xs)) where
    inner j x xs = unfoldr xs 



permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations x : xs b p
