{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
--import ITMOPrelude.Primitive
--import ITMOPrelude.List

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show,Read)

-- создание пустого дерева
createTree :: Tree a
createTree = Leaf

-- вставка элемента в вершину
insertToNode :: Tree a -> a -> Tree a
insertToNode Leaf x = Node x Leaf Leaf
insertToNode (Node n l r) x = Node x l r

-- вставка элемента в качестве самого левого
insertToLeft :: Tree a -> a -> Tree a
insertToLeft Leaf x = Node x Leaf Leaf
insertToLeft (Node n l r) x = insertToLeft l x

-- вставка элемента в качестве самого правого
insertToRight :: Tree a -> x -> Tree a
insertToRigth Leaf x = Node x Leaf Leaf
insertToRight (Node n l r) x = insertToRight r x

-- левый поворот
leftRotation :: Tree a -> Tree a
leftRotation Leaf = Leaf
leftRotation (Node n l Leaf) = Node n l Leaf
leftRotation (Node n l (Node r rl ll)) = Node r (Node n l rl) ll

-- правый поворот
rightRotation :: Tree a -> Tree a
rightRotation Leaf = Leaf
rightRotation (Node n Leaf r) = Node n Leaf r
rightRotation (Node n (Node l ll lr) r) = Node l ll (Node n lr r)

-- map
map :: (a -> b) -> Tree a -> Tree b
map f Leaf = Leaf
map f (Node a l r) = Node (f a) (map f l) (map f r)

-- foldr
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f z Leaf = z
foldr f z (Node a l r) = foldr f (f a (foldr f z r)) l