module Examples.Control.Monad.Indexed.Linear.DVDDrive

import Control.Monad.Indexed.Linear
import Data.Linear.Notation 

||| Conor's DVDDrive indexed monad example now with lolipops! Well curly whorlies
||| since we can't have unicode :)

DVD : Type
DVD = Unit

data DriveState = Empty | Full

data DVDDrive : DriveState -> DriveState -> Type -> Type where
  DReturn : a -@ DVDDrive i i a
  DInsert : DVD -@ DVDDrive Full k a -@ DVDDrive Empty k a
  DEject  : (DVD -@ DVDDrive Empty k a) -@ DVDDrive Full k a
  

dInsert : (1 _ : DVD) -> DVDDrive Empty Full ()
dInsert dvd = DInsert dvd $ DReturn ()

dEject : DVDDrive Full Empty DVD
dEject = DEject $ \ dvd => DReturn dvd

||| monadic equipment so we can write DVDDrive programs in familiar
||| notation.
implementation LixMonad DriveState DVDDrive where
  lireturn = DReturn
  libind (DReturn a)     k = k a
  libind (DInsert dvd j) k = DInsert dvd (libind j k)  
  libind (DEject j)      k = DEject $ \dvd => libind (j dvd) k
  
(>>=) : DVDDrive i j a -@ (a -@ DVDDrive j k b) -@ DVDDrive i k b
(>>=) = libind

(>>) : DVDDrive i j () -@ DVDDrive j k b -@ DVDDrive i k b
i >> k = i `libind` (\() => k) 

return : a -@ DVDDrive i i a 
return = lireturn


{- EXAMPLE Programs -}

-- This should typecheck. 
discSwap : DVD -@ DVDDrive Full Full DVD
discSwap dvd = do
  dvd' <- dEject
  dInsert dvd
  return dvd'

-- But not this due to wrong state transition 
failing "When unifying"
  discSwapOops : DVD -@ DVDDrive Full Full DVD
  discSwapOops dvd = do
    dInsert dvd
    dEject

-- Even though the types line up linearity should forbid the drive cloning the DVD!
failing "Trying to use linear name dvd' in non-linear context"
  discSwapClone : DVD -@ DVDDrive Full Full (DVD, DVD)
  discSwapClone dvd = do
    dvd' <- dEject
    dInsert dvd
    return (dvd', dvd')

-- Or return the wrong DVD i.e. not use the one that was ejected. 
failing "There are 0 uses of linear name dvd'"
  discSwapEh : DVD -@ DVDDrive Full Full DVD
  discSwapEh dvd = do
    dvd' <- dEject
    dInsert dvd
    return dvd 

  
  
