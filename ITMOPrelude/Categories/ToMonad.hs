{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Из этих
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

id a = a

-- делаем эту                                 

instance MonadJoin m => Monad m where
	return = returnJoin
	ma >>= f = join (fmap f ma)
	
instance MonadFish m => Monad m where
	return = returnFish
	ma >>= f = id ma >=> f