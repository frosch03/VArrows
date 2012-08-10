{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow
where

import Prelude hiding (id, (.))

import Control.Category

-- Vectors 
import Vectors


--infixr 5 <+>
infixr 3 ***
infixr 3 &&&
--infixr 2 +++
--infixr 2 |||
--infixr 1 ^>>, >>^
--infixr 1 ^<<, <<^


class (Category a) => Arrow a where
    arr    :: (VNat n) 
           => (Vec n b -> Vec n b) -> a (Vec n b) (Vec n b)

    firsts :: (VNat n, VNat nj, VLs n nj VTrue)
           => n -> a (Vec n b) (Vec n b) -> a (Vec nj b) (Vec nj b)

    lasts  :: (VNat n, VNat nj, VLs  n nj VTrue) 
           => n -> a (Vec n b) (Vec n b) -> a (Vec nj b) (Vec nj b)

    (***)  :: (VNat n, VNat j, VAdd n j nj) 
           => a (Vec n b) (Vec n b) -> a (Vec j b) (Vec j b) -> a (Vec nj b) (Vec nj b)

    (&&&)  :: (VNat n, VAdd n n nn) 
           => a (Vec n b) (Vec n b) -> a (Vec n b) (Vec n b) -> a (Vec n b) (Vec nn b)
