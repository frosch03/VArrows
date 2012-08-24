module Control.Category
where

import qualified Prelude

infixr 9 . 
infixr 1 >>>, <<<

class Category cat where
    id  :: cat b b
    (.) :: cat c d -> cat b c -> cat b d




instance Category (->) where
    id  = Prelude.id
    (.) = (Prelude..)



(>>>) :: (Category cat) => cat b c -> cat c d -> cat b d
f >>> g = g . f

(<<<) :: (Category cat) => cat c d -> cat b c -> cat b d
(<<<) = (.)
