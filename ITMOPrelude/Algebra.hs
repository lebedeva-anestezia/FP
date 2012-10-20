{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

import ITMOPrelude.Primitive
                                
class Monoid m where
	mempty :: m      
	mappend :: m -> m -> m

class Monoid m => Group m where
	ginv :: m -> m
                                

instance Monoid Nat where
	mempty = Zero
 	mappend = (+.)
                                                        
instance Monoid Int where
	mempty = intZero
	mappend = (.+.)

instance Group Int where
	 ginv = intNeg

data MulInt = Mult Int

instance Monoid MulInt where
	mempty = Mult intOne
	(Mult a) `mappend` (Mult b) = Mult $ a .*. b 

instance Monoid Rat where
	mempty = Rat intZero natOne
	mappend = (%+)

data MulRat = RMult Rat

instance Monoid MulRat where
	mempty = RMult $ Rat intOne natOne
	(RMult a) `mappend` (RMult b) = RMult $ a %* b
                
instance Group Rat where
	ginv = ratNeg

instance Group MulRat where
	ginv (RMult a) =  RMult $ ratInv a