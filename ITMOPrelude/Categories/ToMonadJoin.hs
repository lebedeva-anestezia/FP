{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin

-- Из этих
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

id a = a

-- делаем эту

instance Monad m => MonadJoin m where
	returnJoin = return
	join mma = mma >>= id
	

instance MonadFish m => MonadJoin m where
	returnJoin = returnFish
	join = id >=> id