{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда

instance Category (->) where
	id = \x -> x
	f . g = \x -> f (g x)

instance Functor List where
	fmap = map

instance Functor Tree where 
	fmap = tmap

                                      
instance Monad List where
	return a = Cons a Nil
	a >>= f = concatMap f a


--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return x = State (\s -> (x, s))
    (State h) >>= f = State $ \s -> let (newstate, a) = h s
				(State g) = f a
			in g newstate	
