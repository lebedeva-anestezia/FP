{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.List
import ITMOPrelude.Tree
import Prelude(error)


class Category f where
	arrow :: (a -> b) -> f a b
--	id :: 
	(.) :: f b c -> f a b -> f a c
                       
class Functor f where
	fmap :: (a -> b) -> f a -> f b

instance Functor List where
	fmap = map

instance Functor Tree where 
	fmap = tmap


class Monade m where
	return :: a -> m a	
	(>>=) :: m a -> (a -> m b) -> m b
	(>>) :: m a -> m b -> m b
	x >> y = x >>= \_ -> y 

instance Monade List where
	return a = Cons a Nil
	a >>= f = concatMap f a