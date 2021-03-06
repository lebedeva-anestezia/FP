{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show,Read)

-- �������� ������� ������
createTree :: Tree a
createTree = Leaf

-- ������� �������� � �������
insertToNode :: Tree a -> a -> Tree a
insertToNode Leaf x = Node x Leaf Leaf
insertToNode (Node n l r) x = Node x l r

-- ������� �������� � �������� ������ ������
insertToLeft :: Tree a -> a -> Tree a
insertToLeft Leaf x = Node x Leaf Leaf
insertToLeft (Node n l r) x = insertToLeft l x

-- ������� �������� � �������� ������ �������
insertToRight :: Tree a -> x -> Tree a
insertToRigth Leaf x = Node x Leaf Leaf
insertToRight (Node n l r) x = insertToRight r x

-- ����� �������
leftRotation :: Tree a -> Tree a
leftRotation Leaf = Leaf
leftRotation (Node n l Leaf) = Node n l Leaf
leftRotation (Node n l (Node r rl ll)) = Node r (Node n l rl) ll

-- ������ �������
rightRotation :: Tree a -> Tree a
rightRotation Leaf = Leaf
rightRotation (Node n Leaf r) = Node n Leaf r
rightRotation (Node n (Node l ll lr) r) = Node l ll (Node n lr r)

-- map
tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf = Leaf
tmap f (Node a l r) = Node (f a) (tmap f l) (tmap f r)

-- foldr
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f z Leaf = z
foldr f z (Node a l r) = foldr f (f a (foldr f z r)) l