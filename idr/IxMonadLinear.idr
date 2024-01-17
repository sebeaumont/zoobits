module IxMonadLinear

import Data.Linear.Notation

||| I try and try to make this work
interface IxMonad (m : Type -> Type -> Type -> Type) where
  ireturn : a -> m i i a
  ibind : m i j a -> (a -> m j k b) -> m i k b
  
||| So let's see if we can implement one...  
data Thing = Unit

data State = Empty | Full 

data FK : i -> j -> Type -> Type where
  FKreturn : a -> FK i i a
  FKinsert : Thing -> FK Full k a -> FK Empty k a
  
||| I try and try and try...
implementation IxMonad FK where
  ireturn = FKreturn
  ibind (FKreturn a) k = k a
  ibind (FKinsert a j) k = ?fk 
  -- this don't TC: FKinsert a (ibind j k)
  -- seems like Idris needs some more info about k here.
  -- and the proof explorer don't work in emacs. Ah well. 
  -- at least the concrete IxMonadLinearLollipop is OK.
  
  
  
