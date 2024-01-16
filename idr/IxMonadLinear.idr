module IxMonadLinear



||| Conor's DVDDrive indexed monad example linearised a.k.a. spot the lolipops?

DVD : Type
DVD = Unit

data DriveState = Empty | Full

-- I tried to do IxMonad interface and implementation for this but gave up
-- in despair.

data DVDDrive : DriveState -> DriveState -> Type -> Type where
  DReturn : (1 _ : a) -> DVDDrive i i a
  DInsert : (1 _ : DVD) -> (1 _ : DVDDrive Full k a) ->
                        DVDDrive Empty k a
  DEject  : (1 _ : ((1 _ : DVD) -> DVDDrive Empty k a)) -> 
                        DVDDrive Full k a
  

dInsert : (1 _ : DVD) -> DVDDrive Empty Full ()
dInsert dvd = DInsert dvd $ DReturn ()

dEject : DVDDrive Full Empty DVD
dEject = DEject $ \ dvd => DReturn dvd

||| monadic means for our machine

ireturn : (1 _ : a) -> DVDDrive i i a
ireturn = DReturn 

ibind : (1 _ : DVDDrive i j a) -> (1 _ : ((1 _ : a) -> DVDDrive j k b)) -> DVDDrive i k b
ibind (DReturn a)     k = k a
ibind (DInsert dvd j) k = DInsert dvd (ibind j k)  
ibind (DEject j)      k = DEject $ \dvd => ibind (j dvd) k

(>>=) : (1 _ : DVDDrive i j a) -> (1 _ : ((1 _ : a) -> DVDDrive j k b)) -> DVDDrive i k b
(>>=) = ibind

(>>) : (1 _ : DVDDrive i j ()) -> (1 _ : DVDDrive j k b) -> DVDDrive i k b
i >> k = i `ibind` (\() => k) 

return : (1 _ : a) -> DVDDrive i i a 
return = ireturn

{- EXAMPLE Programs -}

-- This should typecheck. 

discSwap : (1 _ : DVD) -> DVDDrive Full Full DVD
discSwap dvd = do
  dvd' <- dEject
  dInsert dvd
  return dvd'

{-
-- but not this - has to be said the error messages are better for
-- quantitive type misuse but we are in the home of QTT

discSwapOops : DVD -> DVDDrive Full Full DVD
discSwapOops dvd = do
  dInsert dvd
  dEject

-- Even though the types line up linearity should forbid the drive cloning the DVD!

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
-}

  
  
