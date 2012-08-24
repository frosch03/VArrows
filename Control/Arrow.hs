{-# LANGUAGE TypeOperators #-}
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


class (Category a) => VArrow a where
    arr    :: (Vec n b -> Vec n b) -> a (Vec n b) (Vec n b)


    init   :: a (Vec n b) (Vec n b) -> a (Vec (n :- VNat1) b) (Vec (n :- VNat1) b)

    tail   :: a (Vec n b) (Vec n b) -> a (Vec (n :- VNat1) b) (Vec (n :- VNat1) b)

    head   :: a (Vec n b) (Vec n b) -> a (Vec VNat1 b) (Vec VNat1 b)

    last   :: a (Vec n b) (Vec n b) -> a (Vec VNat1 b) (Vec VNat1 b)


    (***)  :: a (Vec n b) (Vec n b) -> a (Vec m b) (Vec m b) -> a (Vec (n :+ m) b) (Vec (n :+ m) b)

    (&&&)  :: a (Vec n b) (Vec n b) -> a (Vec n b) (Vec n b) -> a (Vec n b) (Vec (n :+ n) b)
