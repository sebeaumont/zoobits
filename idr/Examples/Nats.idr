module Examples.Nats

{- Proof in Idris2 exercises -}

%default total

{- Nice proof of commutativity of Nat addition -}

plus_commutes_Z : (m : Nat) -> m = plus m 0
plus_commutes_Z 0 = Refl
plus_commutes_Z (S k) = 
  let rec = plus_commutes_Z k in 
    -- Nota. sym is required as we have a proof in rec that `k = plus
    -- k 0` but we need to replace `plus K 0` with `k` in the rewrite
    -- so that `S k = S k`.
    rewrite sym rec in Refl 

plus_commutes_S : (k : Nat) -> (m : Nat) -> S (plus m k) = plus m (S k)
plus_commutes_S k 0 = Refl
plus_commutes_S k (S j) = rewrite plus_commutes_S k j in Refl

plus_commutes : (n : Nat) -> (m : Nat) -> n + m = m + n
plus_commutes 0 m = plus_commutes_Z m
plus_commutes (S k) m = 
  rewrite plus_commutes k m in plus_commutes_S k m

{- Not quite so beautiful proof (by contradiction) of the 
   decidable equality of Nats -}

-- we require 3 falsehood lemmas:
    
||| zero is not a successor of any n (1)
zeroNotSucc : (prf : Z = (S n)) -> Void
zeroNotSucc Refl impossible

||| the successor of n is not Z (2)
succNotZero : (prf : (S n) = Z) -> Void
succNotZero Refl impossible

{- this makes me think that there may be a general congruence for falsehood.. -}
||| if n = m is false then we must imply that S n = S m is also false (3)
succDiffers : (contra : k = j -> Void) -> (impl : (S k) = (S j)) -> Void
succDiffers contra Refl = contra Refl


||| we can then decide if Nats are equal
natsEqual : (n : Nat) -> (m : Nat) -> Dec (n = m)
natsEqual Z Z = Yes Refl
natsEqual Z (S m) = No zeroNotSucc
natsEqual (S m) Z = No succNotZero
natsEqual (S n) (S m) = 
  case natsEqual n m of 
    (Yes prf) => Yes (cong S prf)
    (No contra) => No (succDiffers contra)
