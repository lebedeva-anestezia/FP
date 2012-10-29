{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- делаем из нас
instance MonadFish m => Monad m where
    return = returnFish
    ma >>= f = id ma >=> f

instance MonadFish m => Functor m where
    fmap f ma = (returnFish . f >=> id) ma

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id
