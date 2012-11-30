module Tree where

import Data.Array
import Utils
import Data.List
import Data.Ix

data Tree a = Tree a [Tree a] deriving (Eq,Ord,Show)

treeData :: Tree a -> a
treeData (Tree a _) = a

treeChildren :: Tree a -> [Tree a]
treeChildren (Tree _ children) = children

instance Functor Tree where
  fmap f (Tree a chds) = Tree (f a) (map (fmap f) chds)

treeFold :: (a -> b -> a) -> a -> Tree b -> a
treeFold f x (Tree a children) = f (foldl (treeFold f) x children) a

treeMapAccum :: (c -> a -> (c,b)) -> c -> Tree a -> (c,Tree b)
treeMapAccum f counter (Tree a children) = (finalC,Tree topA newChildren)
  where (newCounter, newChildren) = (mapAccumL (treeMapAccum f) counter children)
        (finalC,topA) = f newCounter a
        
enumerateTree :: Tree a -> (Tree (Int,a), Array Int (Tree (Int,a)))
enumerateTree t = (numberedT, toArray $ toList $ numberedT)
  where numberedT = snd $ treeMapAccum (\c x -> (c+1,(c,x))) 0 t
        
nodeNumber :: Tree (Int,a) -> Int
nodeNumber (Tree (n,_) _) = n

toList :: Tree a -> [Tree a]
toList (Tree a ts) = (concatMap toList ts) ++ [Tree a ts]

leftMostLeafDescendant :: Tree a -> Tree a
leftMostLeafDescendant = head.toList

lmld :: Tree a -> [Tree a]
lmld (Tree a []) = [Tree a []]
lmld (Tree a children) = (leftMostDesc:rest) ++ [leftMostDesc]
  where (leftMostDesc:rest) = (concatMap lmld children) 

lmldTest t = (map leftMostLeafDescendant $ toList t) == (lmld t)

keyRoots :: Tree a -> [Tree a]
keyRoots t = (kr t) ++ [t]
  where kr (Tree a children) = (concatMap kr children) ++ (tail children)

size :: Tree a -> Int
size = length.toList
        
type Omega = (Double,Double,Double)

               -- Omegas,first index, second index, array of tree1, array of tree 2, tree distance array -> indexes for tree distance array
forestDistance :: Omegas -> Int -> Int -> Array Int (Tree (Int,a)) -> Array Int (Tree (Int,a))
                  -> Array (Int,Int) Double -> [((Int,Int),Double)]
forestDistance (del,ins,ren) i j t1Array t2Array treeDistArray = leftPaths
  where li = nodeNumber $ leftMostLeafDescendant $ t1Array!i
        lj = nodeNumber $ leftMostLeafDescendant $ t2Array!j
        fdarray = array ((li-1,lj-1),(i,j)) $
                  [((li-1,lj-1),0)] ++
                  [((di,lj-1),del + (fdarray!(di-1,lj-1))) | di <- [li..i]] ++
                  [((li-1,dj),ins + (fdarray!(li-1,dj-1))) | dj <- [li..i]] ++
                  leftPaths ++ nonLeftPaths
        (lefts,nlefts) = partition (\(di,dj) -> (li,lj) == (nodeNumber $ leftMostLeafDescendant $ t1Array!di,nodeNumber $ leftMostLeafDescendant $ t2Array!dj))
                         [(di,dj) <- di <- [l1..i], dj <- [lj..j]]
        leftPaths = 
                  