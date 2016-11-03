{-# LANGUAGE BangPatterns #-}
module MiscUtil where

import Control.Monad

-- ahkera mapM
mapM' !f !as = sequence' (map' f as)
    where
      map' _ [] = []
      map' f !(x:xs) = f x : map' f xs
      sequence' !ms = foldr k (return []) ms
          where
            k !m !m' = do { x <- m; xs <- m'; return $! (x:xs) }

