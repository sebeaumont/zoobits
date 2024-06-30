module Control.Monad.Indexed.Linear

import Data.Linear.Notation 

||| LixMonad the linear indexed monad interface.
|||
public export
interface LixMonad x m | m where
  lireturn : {0 i : x} -> a -@ m i i a
  libind   : {0 i,j,k : x} -> m i j a -@ (a -@ m j k b) -@ m i k b
  -- default composition
  licomp   : {0 i,j,k : x} -> (b -@ m j k c) -> (a -@ m i j b) -> a -@ m i k c
  licomp f g = \a => libind (g a) f

