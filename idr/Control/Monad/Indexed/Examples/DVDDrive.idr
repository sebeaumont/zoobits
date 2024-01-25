module Control.Monad.Indexed.Examples.DVDDrive

import Control.Monad.Indexed

{- Conor's DVDDrive example on SO was the motivation for this. As it
   forbids wrong states in the types; later we will use linear types
   to make this more robust from programming errors. -}

||| The DVD is just a thing.
DVD : Type
DVD = Unit

||| There are the allowed states of the drive and the type of the state machine.
data DriveState = Empty | Full

||| The DVD drive may be programmed via functions that specify 
||| the permitted state transitions.
data DVDDrive : DriveState -> DriveState -> Type -> Type where
  DReturn : a -> DVDDrive i i a
  DInsert : DVD -> DVDDrive Full k a ->
                   DVDDrive Empty k a
  DEject : (DVD -> DVDDrive Empty k a) -> 
                   DVDDrive Full k a


dInsert : DVD -> DVDDrive Empty Full ()
dInsert dvd = DInsert dvd $ DReturn ()

dEject : DVDDrive Full Empty DVD
dEject = DEject $ \ dvd => DReturn dvd

    
||| Monadic machinery for writing DVDDrive programs. Providing
||| a effectful embedded DSL (eDSL) for DVDDrive programming where
||| actions may only be performed wrt to allowed states.
implementation IxMonad DriveState DVDDrive where
  ireturn = DReturn             
  ibind (DReturn a)     k  = k a
  ibind (DInsert dvd j) k  = DInsert dvd (ibind j k)
  ibind (DEject j)      k  = DEject $ \dvd => ibind (j dvd) k

||| These for the convenience of `do` notation.
(>>=) : DVDDrive i j a -> (a -> DVDDrive j k b) -> DVDDrive i k b
(>>=) = ibind

(>>) : DVDDrive i j a -> DVDDrive j k b -> DVDDrive i k b
i >> k = i `ibind` (\_ => k) 

return : a -> DVDDrive i i a 
return = ireturn

{- EXAMPLE Programs -}

||| This program  works as it should
discSwap : DVD -> DVDDrive Full Full DVD
discSwap dvd = do
  dvd' <- dEject
  dInsert dvd
  return dvd'
 
{- This program correctly fails to typecheck refusing to unify Full with Empty -}
failing "When unifying" 
  discSwapOops : DVD -> DVDDrive Full Full DVD
  discSwapOops dvd = do
      dInsert dvd
      dEject


-- Even though the types line up, linearity can forbid the drive cloning the DVD!
-- See: IndexedLinear for a further use of such type-safety.
discSwapClone : DVD -> DVDDrive Full Full (DVD, DVD)
discSwapClone dvd = do
  dvd' <- dEject
  dInsert dvd
  return (dvd', dvd')

-- Or return the wrong DVD?

discSwapEh : DVD -> DVDDrive Full Full DVD
discSwapEh dvd = do
  dvd' <- dEject
  dInsert dvd
  return dvd 

 
  

  
  
