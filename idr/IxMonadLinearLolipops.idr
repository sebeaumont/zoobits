module IxMonadLinearLolipops

import Data.Linear.Notation 

||| Conor's DVDDrive indexed monad example linearised a.k.a. spot the lolipops?

DVD : Type
DVD = Unit

data DriveState = Empty | Full

-- I tried to do IxMonad interface and implementation for this but gave up
-- in despair.

data DVDDrive : DriveState -> DriveState -> Type -> Type where
  DReturn : a -@ DVDDrive i i a
  DInsert : DVD -@ DVDDrive Full k a -@ DVDDrive Empty k a
  DEject  : (DVD -@ DVDDrive Empty k a) -@ DVDDrive Full k a
  

dInsert : (1 _ : DVD) -> DVDDrive Empty Full ()
dInsert dvd = DInsert dvd $ DReturn ()

dEject : DVDDrive Full Empty DVD
dEject = DEject $ \ dvd => DReturn dvd

||| monadic means for our machine

ireturn : a -@ DVDDrive i i a
ireturn = DReturn 

ibind : DVDDrive i j a -@ (a -@ DVDDrive j k b) -@ DVDDrive i k b
ibind (DReturn a)     k = k a
ibind (DInsert dvd j) k = DInsert dvd (ibind j k)  
ibind (DEject j)      k = DEject $ \dvd => ibind (j dvd) k

(>>=) : DVDDrive i j a -@ (a -@ DVDDrive j k b) -@ DVDDrive i k b
(>>=) = ibind

(>>) : DVDDrive i j () -@ DVDDrive j k b -@ DVDDrive i k b
i >> k = i `ibind` (\() => k) 

return : a -@ DVDDrive i i a 
return = ireturn

{- EXAMPLE Programs -}

-- This should typecheck. 

discSwap : DVD -@ DVDDrive Full Full DVD
discSwap dvd = do
  dvd' <- dEject
  dInsert dvd
  return dvd'


-- but not this 
{-
discSwapOops : DVD -@ DVDDrive Full Full DVD
discSwapOops dvd = do
  dInsert dvd
  dEject

-- Even though the types line up linearity should forbid the drive cloning the DVD!

discSwapClone : DVD -@ DVDDrive Full Full (DVD, DVD)
discSwapClone dvd = do
  dvd' <- dEject
  dInsert dvd
  return (dvd', dvd')

-- Or return the wrong DVD?

discSwapEh : DVD -@ DVDDrive Full Full DVD
discSwapEh dvd = do
  dvd' <- dEject
  dInsert dvd
  return dvd 
-}
  
  
