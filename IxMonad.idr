module IxMonad

-- I tried to do IxMonad interface and implementation for this but gave up
-- in despair.

{-
interface IxMonad m where
  ireturn : a -> m i i a
  ibind   : m i j a -> (a -> m j k b) -> m i k b
  icomp : (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
  icomp f g = \a => ibind (g a) f
-}

||| Conor's DVDDrive example

DVD : Type
DVD = Unit

data DriveState = Empty | Full

data DVDDrive : DriveState -> DriveState -> Type -> Type where
  DReturn : a -> DVDDrive i i a
  DInsert : DVD -> DVDDrive Full k a ->
                   DVDDrive Empty k a
  DEject : (DVD -> DVDDrive Empty k a) -> 
                   DVDDrive Full k a
  
{-
implementation IxMonad DVDDrive where
  ireturn = DReturn             
  ibind (DReturn a)     k  = k a
  ibind (DInsert dvd j) k  = DInsert dvd (ibind j k)
  ibind (DEject j)      k  = DEject j $ \ dvd => ibind (j dvd) k
-}


dInsert : DVD -> DVDDrive Empty Full ()
dInsert dvd = DInsert dvd $ DReturn ()

dEject : DVDDrive Full Empty DVD
dEject = DEject $ \ dvd => DReturn dvd

||| monadic means for our machine

ireturn : a -> DVDDrive i i a
ireturn = DReturn 

ibind : DVDDrive i j a -> (a -> DVDDrive j k b) -> DVDDrive i k b
ibind (DReturn a)     k = k a
ibind (DInsert dvd j) k = DInsert dvd (ibind j k)  
ibind (DEject j)      k = DEject $ \dvd => ibind (j dvd) k

(>>=) : DVDDrive i j a -> (a -> DVDDrive j k b) -> DVDDrive i k b
(>>=) = ibind

(>>) : DVDDrive i j a -> DVDDrive j k b -> DVDDrive i k b
i >> k = i `ibind` (\_ => k) 

return : a -> DVDDrive i i a 
return = ireturn

{- EXAMPLE Programs -}

||| This one works as it should
discSwap : DVD -> DVDDrive Full Full DVD
discSwap dvd = do
  dvd' <- dEject
  dInsert dvd
  return dvd'
 
||| This won't typcheck as state machine forbids this seqeunce of actions
||| even though it returns a DVD.

{-
discSwapOops : DVD -> DVDDrive Full Full DVD
discSwapOops dvd = do
  dInsert dvd
  dEject
-}

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

 
{- OK
--- TODO make this IO monad not concrete
---  
data IxState : (ini : Type) -> (fin : Type) -> (res : Type) -> Type where
  MkISM : (ini -> IO (fin, res)) -> IxState ini fin res

runIxState : (_ : ini) -> (_ : IxState ini fin res) -> IO (fin, res)
runIxState i (MkISM f) = f i


(>>=) : IxState i f a -> (a -> IxState f r b) -> IxState i r b
v >>= g = MkISM $ \i =>  
      runIxState i v >>= \(a, m) => 
        runIxState a (g m)
-}    
  

  
  
