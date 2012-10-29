{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- делаем из нас

instance MonadJoin m => Monad m where
	return = returnJoin
	ma >>= f = join . fmap f ma

instance MonadJoin m => MonadFish m where
	returnFish = returnJoin
	f >=> g = \x -> join . fmap g (f x)