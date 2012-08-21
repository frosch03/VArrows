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


    init   :: ( VNat n, VNat m
              , VAdd n m nm
              , VEq (VSucc n) nm VTrue
              )
           => a (Vec nm b) (Vec nm b) -> a (Vec n b) (Vec n b)

    tail   :: ( VNat n, VNat m
              , VAdd n m nm
              , VEq nm (VSucc m) VTrue
              )
           => a (Vec nm b) (Vec nm b) -> a (Vec m b) (Vec m b)

    head   :: ( VNat n, VNat m
              , VAdd n m nm
              , VEq nm (VSucc m) VTrue
              )
           => a (Vec nm b) (Vec nm b) -> a (Vec n b) (Vec n b)

    last   :: ( VNat n, VNat m
              , VAdd n m nm
              , VEq (VSucc n) nm VTrue
              )
           => a (Vec nm b) (Vec nm b) -> a (Vec m b) (Vec m b)


    (***)  :: (VNat n, VNat j, VAdd n j nj) 
           => a (Vec n b) (Vec n b) -> a (Vec j b) (Vec j b) -> a (Vec nj b) (Vec nj b)

    (&&&)  :: (VNat n, VAdd n n nn) 
           => a (Vec n b) (Vec n b) -> a (Vec n b) (Vec n b) -> a (Vec n b) (Vec nn b)
