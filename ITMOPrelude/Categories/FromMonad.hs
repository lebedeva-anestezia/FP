{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем нас

instance Monad m => Functor m
	fmap f ma = ma >>= (return . f)

instance Monad m => MonadJoin m
	returnJoin = return
	join mma = mma >>= id

instance Monad m => MonadFish m
	returnFish = return
	f >=> g = (\x -> f x) >>= g