{-# LANGUAGE GADTs, TypeOperators #-}
module VArrow 
where

import Control.Category
import Control.Arrow

import Vectors


--instance (VNat n) => Category (Vec n a) where
--    id  = id
--    (.) (Vec (VSucc _) x1) (Vec n2 x2) 


vAppend :: Vec n b -> Vec m b -> Vec (n :+ m) b
vAppend T v         = v 
vAppend (x :. xs) v = x :. (vAppend xs v)
