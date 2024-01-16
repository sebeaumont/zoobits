{-# LANGUAGE TypeData, DataKinds, MonoLocalBinds, GADTs, QualifiedDo #-}
module Control.Monad.IxMonadExample where

import qualified Control.Monad.IxMonad as I
import Control.Monad.IxMonad (IxMonad, ibind, ireturn)
import Data.Kind

-- Conor's DVD drive example
data DVD

data DVDDrive :: Bool -> Bool -> Type -> Type where  -- Bool is "drive full?"
  DReturn :: a -> DVDDrive i i a
  DInsert :: DVD ->                   -- you have a DVD
             DVDDrive True k a ->     -- you know how to continue full
             DVDDrive False k a       -- so you can insert from empty
  DEject  :: (DVD ->                  -- once you receive a DVD
              DVDDrive False k a) ->  -- you know how to continue empty
             DVDDrive True k a        -- so you can eject when full

instance IxMonad DVDDrive where  
  ireturn = DReturn              
  ibind (DReturn a)     k  = k a
  ibind (DInsert dvd j) k  = DInsert dvd (ibind j k)
  ibind (DEject j)      k  = DEject $ \dvd -> ibind (j dvd) k

  
dInsert :: DVD -> DVDDrive False True ()
dInsert dvd = DInsert dvd $ DReturn ()

dEject :: DVDDrive True False DVD
dEject = DEject $ \ dvd -> DReturn dvd

{-
discSwap :: DVD -> DVDDrive True True DVD
discSwap dvd = dEject `ibind` \dvd' -> dInsert dvd `ibind` \_ -> ireturn dvd'
-}
discSwap :: DVD -> DVDDrive True True DVD
discSwap dvd = I.do
  dvd' <- dEject
  dInsert dvd
  ireturn dvd'


{-
discSwapOops :: DVD -> DVDDrive True True DVD
discSwapOops dvd = I.do dInsert dvd; dEject 
-}
