||| A module of operators and vectors
module Data.Algebra

import Data.Vect
import Data.Fin

import Math.Complex
import Math.Rational

-- This is our field of choice rather than double as it looks nicer and
-- can be computed to arbitrary precision.

||| Shorthand for Complex Rational
QQ : Type
QQ = Complex Rational 

||| Shorthand constructor
qq : Rational -> Rational -> QQ
qq = complex 

{- 
  TODO we need to define:
  norms, dot products, kets, bras, brackets and operators
  or perhaps we need a new approach:
  
  https://arxiv.org/abs/2201.02246
-}

-- do we really need this extra layer of abstraction here?
data Matrix : Nat -> Nat -> Type where
  Mat : {m : Nat} -> {n : Nat} -> Vect m (Vect n QQ) -> Matrix m n


||| A Ket is a column vector
export
data Ket : Nat -> Type where
  KVect : {d : Nat} -> Vect d (Vect 1 QQ) -> Ket d

||| Make a ket from d complex rationals.
export
ket : {d : Nat} -> Vect d (Vect 1 QQ) -> Ket d
ket = KVect

||| A Bra is a row vector
export
data Bra : Nat -> Type where   
  BVect : {d : Nat} -> Vect 1 (Vect d QQ) -> Bra d

||| Make a bra from d complex rationals
export 
bra : {d : Nat} -> Vect 1 (Vect d QQ) -> Bra d
bra = BVect

-- extract the raw vectors
ket2vec : Ket d -> Vect d QQ
ket2vec (KVect v) = head $ transpose v

bra2vec : Bra d -> Vect d QQ
bra2vec (BVect v) = head v

||| Inner (.) product.  
export
braket : Bra d -> Ket d -> QQ
braket b k = 
  foldr (+) (qq 0 0) $ zipWith (*) (bra2vec b) (ket2vec k)


compBasis0 : Ket 2  
compBasis0 = ket [[qq 1 0],[qq 0 0]]

