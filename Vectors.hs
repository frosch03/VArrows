{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Vectors 
where

-- This is the datatype definition of a vector 
data Vec n a where
    T    :: Vec VZero a
    (:.) :: (VNat n) => a -> (Vec n a) -> (Vec (VSucc n) a)

infixr 6 :.



-- The Type level Arithmetics start here 

-- Bools 
class VBool b

data VTrue
instance VBool VTrue

data VFalse
instance VBool VFalse


vTrue :: VTrue
vTrue  = undefined

vFalse :: VFalse
vFalse  = undefined



-- Natural Numbers:
class VNat n

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



-- Addition 
class (VNat a, VNat b, VNat ab) 
    => VAdd a b ab | a b -> ab, a ab -> b
    where vAdd :: a -> b -> ab

instance (VNat b) 
    => VAdd VZero b b
    where vAdd _ j = j

instance (VAdd n j nj) 
    => VAdd (VSucc n) j (VSucc nj)
    where vAdd n j = vSucc $ vAdd (vPred n) j


-- LessThen check (<=)
class (VNat a, VNat b, VBool ab) 
    => VLt a b ab | a b -> ab
    where vLt :: a -> b -> ab

instance (VNat n) 
    => VLt VZero (VSucc n) VTrue
    where vLt _ _ = vTrue

instance VLt VZero VZero VTrue
    where vLt _ _ = vTrue

instance (VNat n) 
    => VLt (VSucc n) VZero VFalse
    where vLt _ _ = vFalse

instance (VNat n, VNat j, VLt n j b) 
    => VLt (VSucc n) (VSucc j) b
    where vLt n j = vLt (vPred n) (vPred j)


-- Less check (<)
class (VNat a, VNat b, VBool ab) 
    => VLs a b ab | a b -> ab
    where vLs :: a -> b -> ab

instance (VNat n) 
    => VLs VZero (VSucc n) VTrue
    where vLs _ _ = vTrue

instance VLs VZero VZero VFalse
    where vLs _ _ = vFalse

instance (VNat n) 
    => VLs (VSucc n) VZero VFalse
    where vLs _ _ = vFalse

instance (VNat n, VNat j, VLs n j b) 
    => VLs (VSucc n) (VSucc j) b
    where vLs n j = vLs (vPred n) (vPred j)
