module Utils where

import Data.Array

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

toArray lst = listArray (0,length lst-1) lst