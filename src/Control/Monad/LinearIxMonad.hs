{-# LANGUAGE TypeData, DataKinds #-}
{-# LANGUAGE UnicodeSyntax, LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.LinearIxMonad where

import Data.Kind (Type)


class LinearIxMonad (m :: state -> state -> Type -> Type) where
  ireturnl :: a ⊸ m i i a
  -- the idea here is that the state and the argument can be consumed only once
  ibindl :: m i j a ⊸ (a ⊸ m j k b) ⊸ m i k b

  icompl :: (b ⊸ m j k c) -> (a ⊸ m i j b) -> a ⊸ m i k c
  icompl f g = \a -> ibindl (g a) f

    
-- Can work with qualified do - I return seems not to have sugar tho'
return :: LinearIxMonad m => a ⊸  m i i a 
return = ireturnl

infixl 1 >>= 
(>>=) :: LinearIxMonad m => m i j a ⊸ (a ⊸ m j k b) ⊸ m i k b
(>>=) = ibindl

infixl 1 >>
(>>) :: LinearIxMonad m => m i j () ⊸  m j k b ⊸ m i k b
i >> k = i `ibindl` (\() -> k)
