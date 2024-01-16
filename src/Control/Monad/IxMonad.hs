{-# LANGUAGE TypeData, DataKinds #-}
{-# LANGUAGE UnicodeSyntax, LinearTypes #-}

-- | This to take a look at Conor's implementation. Probably use Ed's library for real.
module Control.Monad.IxMonad
  where

import Prelude hiding ((>>=), (>>), return)
import Data.Kind ( Type )

class IxMonad (m :: state -> state -> Type -> Type) where
  ireturn :: a -> m i i a
  ibind :: m i j a -> (a -> m j k b) -> m i k b
  icomp :: (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
  icomp f g = \a -> ibind (g a) f

-- Can work with qualified do - I return seems not to have sugar tho'
return :: IxMonad m => a ->  m i i a 
return = ireturn

infixl 1 >>= 
(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = ibind

infixl 1 >>
(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
i >> k = i `ibind` \_ -> k  
  


