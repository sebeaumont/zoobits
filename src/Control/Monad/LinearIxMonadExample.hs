{-# LANGUAGE TypeData, DataKinds, GADTs, MonoLocalBinds, QualifiedDo #-}
{-# LANGUAGE UnicodeSyntax, LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.LinearIxMonadExample where

import Data.Kind (Type)
import Prelude.Linear

import Control.Monad.LinearIxMonad (LinearIxMonad, ireturnl, ibindl)
import qualified Control.Monad.LinearIxMonad as LI

-- Conor's DVD drive example indexed monad now with lolipops!  
data DVD
data DriveState = Empty | Full

data DVDDrive :: DriveState -> DriveState -> Type -> Type where  -- Bool is "drive full?"
  DReturn :: a ⊸ DVDDrive i i a
  DInsert :: DVD ⊸                   
             DVDDrive Full k a ⊸     
             DVDDrive Empty k a      
  DEject  :: (DVD ⊸                  
              DVDDrive Empty k a) ⊸  
             DVDDrive Full k a       


instance LinearIxMonad DVDDrive where  
  ireturnl = DReturn              
  ibindl (DReturn a)     k  = k a
  ibindl (DInsert dvd j) k  = DInsert dvd (ibindl j k)
  ibindl (DEject j)      k  = DEject $ \dvd -> ibindl (j dvd) k

  
dInsert :: DVD ⊸ DVDDrive Empty Full ()
dInsert dvd = DInsert dvd $ DReturn ()

dEject :: DVDDrive Full Empty DVD
dEject = DEject $ \ dvd -> DReturn dvd

discSwap :: DVD ⊸ DVDDrive Full Full DVD
--discSwap dvd = dEject `ibindl` \dvd' -> dInsert dvd LI.>> ireturnl dvd'
discSwap dvd = LI.do
  dvd' <- dEject
  dInsert dvd
  LI.return dvd' 

{- This won't typcheck as state machine forbids this seqeunce of actions
   even though it returns a DVD

discSwapOops :: DVD ⊸ DVDDrive Full Full DVD
discSwapOops dvd = LI.do
  dInsert dvd
  dEject

-- Even though the types line up linearity forbids the drive cloning the DVD!

discSwapClone :: DVD ⊸ DVDDrive Full Full (DVD, DVD)
discSwapClone dvd = LI.do
  dvd' <- dEject
  dInsert dvd
  LI.return (dvd', dvd')

-- Or return the wrong DVD?

discSwapEh :: DVD ⊸ DVDDrive Full Full DVD
discSwapEh dvd = LI.do
  dvd' <- dEject
  dInsert dvd
  LI.return dvd 
-}
