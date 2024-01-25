module Control.Monad.Indexed

||| IxMonad the indexed monad interface.
|||
||| The trick here was to provide a type @ x for the indexes.  Nota
||| Bene: There is a nice tower of Functors provided in the `contrib`
||| library `indexed` that provide better abstractions than this. This
||| was just to work it up for myself and the trick above was gleaned
||| from above.
interface IxMonad x m | m where
  ireturn : {0 i : x} -> a -> m i i a
  ibind   : {0 i,j,k : x} -> m i j a -> (a -> m j k b) -> m i k b
  ||| Default implementation of composition
  icomp   : {0 i,j,k : x} -> (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
  icomp f g = \a => ibind (g a) f


 
  

  
  
