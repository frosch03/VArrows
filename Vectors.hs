{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Vectors 
where

--| This is the datatype definition of a vector 
data Vec n a where
    T    :: Vec VZero a
    (:.) :: (VNat n) => a -> (Vec n a) -> (Vec (VSucc n) a)

infixr 6 :.



-- The Type level Arithmetics start here 

class VNat a

data VZero
instance VNat VZero

data VSucc n
instance (VNat n) => VNat (VSucc n)


vZero :: VZero
vZero  = undefined

vSucc :: (VNat n) => n -> VSucc n
vSucc _ = undefined

vPred :: (VNat n) => VSucc n -> n
vPred _ = undefined



class (VNat a, VNat b, VNat ab) => VAdd a b ab | a b -> ab, a ab -> b
    where vAdd :: a -> b -> ab

instance (VNat b) => VAdd VZero b b
    where vAdd _ j = j

instance (VAdd n j nj) => VAdd (VSucc n) j (VSucc nj)
    where vAdd n j = vSucc $ vAdd (vPred n) j
