{-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts, DataKinds, TypeFamilies, TypeOperators #-}
module Vectors 
where

-- This is the datatype definition of a vector 
data Vec n a where
    T    :: Vec VZero a
    (:.) :: a -> Vec n a -> Vec (VSucc n) a


infixr 6 :.



-- The Type level Arithmetics start here 

-- Bools 
data VBool
    = VFalse
    | VTrue


-- Natural Numbers:
data VNat 
    = VZero
    | VSucc (VNat)


type VNat0 = VZero
type VNat1 = VSucc VNat0
type VNat2 = VSucc VNat1
type VNat3 = VSucc VNat2
type VNat4 = VSucc VNat3
type VNat5 = VSucc VNat4
type VNat6 = VSucc VNat5
type VNat7 = VSucc VNat6
type VNat8 = VSucc VNat7
type VNat9 = VSucc VNat8


type family (n::VNat) :+ (m::VNat) :: VNat
type instance VZero   :+ m = m 
type instance VSucc n :+ m = VSucc (n :+ m)

type family (n::VNat) :- (m::VNat) :: VNat
type instance VZero   :- m       = VZero
type instance n       :- VZero   = n
type instance VSucc n :- VSucc m = (n :- m)

type family (n::VNat) := (m::VNat) :: VBool
type instance VZero     := VZero     = VTrue
type instance VZero     := (VSucc m) = VFalse
type instance (VSucc n) := VZero     = VFalse
type instance (VSucc n) := (VSucc m) = (n := m)



vZero :: VNat
vZero  = undefined

vSucc :: VNat -> VNat
vSucc _ = undefined

vPred :: VNat -> VNat
vPred _ = undefined



---- Equals
--class VEq a b eq where 
--    vEq :: VNat -> VNat -> eq 
--
--instance VEq VZero VZero VTrue
--    where vEq _ _ = vTrue
--
--instance (VNat a)
--    => VEq (VSucc a) VZero VFalse
--    where vEq _ _ = vFalse
--
--instance (VNat b)
--    => VEq VZero (VSucc b) VFalse
--    where vEq _ _ = vFalse
--
--instance (VNat a, VNat b, VEq a b eq)
--    => VEq (VSucc a) (VSucc b) (eq) 
--    where vEq a b = vEq (vPred a) (vPred b)


---- Addition 
--class VAdd VNat VNat VNat where 
--    vAdd :: VNat -> VNat -> ab
--
--instance (VNat b) 
--    => VAdd VZero b b
--    where vAdd _ j = j
--
--instance (VNat b) 
--    => VAdd b VZero b
--    where vAdd n _ = n
--
--instance (VAdd n j nj) 
--    => VAdd (VSucc n) j (VSucc nj)
--    where vAdd n j = vSucc $ vAdd (vPred n) j
--
--
---- LessThen check (<=)
--class VLt a b ab where 
--    vLt :: VNat -> VNat -> ab
--
--instance (VNat n) 
--    => VLt VZero (VSucc n) VTrue
--    where vLt _ _ = vTrue
--
--instance VLt VZero VZero VTrue
--    where vLt _ _ = vTrue
--
--instance (VNat n) 
--    => VLt (VSucc n) VZero VFalse
--    where vLt _ _ = vFalse
--
--instance (VNat n, VNat j, VLt n j b) 
--    => VLt (VSucc n) (VSucc j) b
--    where vLt n j = vLt (vPred n) (vPred j)
--
--
---- Less check (<)
--class VLs a b ab where 
--    vLs :: VNat -> VNat -> ab
--
--instance (VNat n) 
--    => VLs VZero (VSucc n) VTrue
--    where vLs _ _ = vTrue
--
--instance VLs VZero VZero VFalse
--    where vLs _ _ = vFalse
--
--instance (VNat n) 
--    => VLs (VSucc n) VZero VFalse
--    where vLs _ _ = vFalse
--
--instance (VNat n, VNat j, VLs n j b) 
--    => VLs (VSucc n) (VSucc j) b
--    where vLs n j = vLs (vPred n) (vPred j)
--
--
---- GreaterThen check (>=)
--class VGt a b ab where 
--    vGt :: VNat -> VNat -> ab
--
--instance (VNat n) 
--    => VGt (VSucc n) VZero VTrue
--    where vGt _ _ = vTrue
--
--instance VGt VZero VZero VTrue
--    where vGt _ _ = vTrue
--
--instance (VNat n) 
--    => VGt VZero (VSucc n) VFalse
--    where vGt _ _ = vFalse
--
--instance (VNat n, VNat j, VGt n j b) 
--    => VGt (VSucc n) (VSucc j) b
--    where vGt n j = vGt (vPred n) (vPred j)
--
--
---- Greater check (>)
--class VGs a b ab where 
--    vGs :: VNat -> VNat -> ab
--
--instance VGs (VSucc n) VZero VTrue
--    where vGs _ _ = vTrue
--
--instance VGs VZero VZero VFalse
--    where vGs _ _ = vFalse
--
--instance VGs VZero (VSucc n) VFalse
--    where vGs _ _ = vFalse
--
--instance VGs (VSucc n) (VSucc j) b
--    where vGs n j = vGs (vPred n) (vPred j)
