||| A module of operators and vectors over the rational complex field we call QQ
||| which hints at the application thereof as well as referring to the field Q
module Math.Algebra.QQ

import Math.Complex
import Math.Rational
import Math.Algebra.Linear.VectorSpace

%default total

||| Shorthand for Complex Rational which is our field for this vector
||| space.
QQ : Type
QQ = Complex Rational 

||| Shorthand constructor
qq : Rational -> Rational -> QQ
qq = complex 


||| Vectors over a field of complex rationals
||| including Scalars, Bras, Kets and square matrices. 
data QVect : Nat -> Type where
  ||| Row vector
  Bra : {d : Nat} -> Vect 1 (Vect d QQ) -> QVect d
  ||| Column or state vector
  Ket : {d : Nat} -> Vect d (Vect 1 QQ) -> QVect d
  ||| Scalar
  Sca : {d : Nat} -> QQ -> QVect d
  ||| 
  Mat : {d : Nat} -> Vect d (Vect d QQ) -> QVect d
 
||| Bra convenience constructor
export
bra : {d : Nat} -> Vect d QQ -> QVect d
bra v = Bra [v]

||| Ket convenience constructor
export
ket : {d : Nat} -> Vect d QQ -> QVect d
ket u = Ket $ transpose [u]

{- TODO check if we need to do so much (un)wrapping here -}
||| Overloaded multiplication - caveat: some of these are not
||| mathematically well defined.
export
multiply : {d : Nat} -> QVect d -> QVect d -> QVect d
multiply (Bra u) (Ket v) = Sca $ dotproduct (head u) (head $ transpose v)
multiply (Bra u) (Sca k) = bra $ map (k *) (head u)
multiply (Bra u) (Bra v) = bra $ zipWith (*) (head u) (head v)
multiply (Bra u) (Mat a) = Bra $ mmul u a 

multiply (Sca k) (Ket v) = ket $ map (k *) (head $ transpose v)
multiply (Sca k) (Bra v) = bra $ map (k *) (head v)
multiply (Sca k) (Sca j) = Sca $ k * j
multiply (Sca k) (Mat a) = Mat $ map (map (k *)) a

multiply (Ket u) (Bra v) = Mat $ mmul u v
multiply (Ket u) (Sca k) = ket $ map (k *) (head $ transpose u)
multiply (Ket u) (Ket v) = ket $ zipWith (*) (head $ transpose u) (head $ transpose v)
multiply (Ket u) (Mat a) = Bra $ mmul (transpose u) a -- ??? conj 

multiply (Mat a) (Mat b) = Mat $ mmul a b
multiply (Mat a) (Sca k) = Mat $ map (map (k *)) a
multiply (Mat a) (Ket v) = Ket $ mmul a v
multiply (Mat a) (Bra v) = Ket $ mmul a (transpose v) -- ??? conj

||| conjugate transpose or adjoint operator
export
dagger : {d : Nat} -> QVect d -> QVect d
dagger (Sca k) = Sca $ conj k
dagger (Bra v) = ket $ map conj (head $ v)
dagger (Ket v) = bra $ map conj (head $ transpose v)
dagger (Mat m) = Mat $ map (map conj) (transpose m)
 
||| vanilla conjugate operator
export
conjugate : {d : Nat} -> QVect d -> QVect d
conjugate (Sca k) = Sca $ conj k  
conjugate (Bra v) = Bra $ map (map conj) v
conjugate (Ket v) = Ket $ map (map conj) v
conjugate (Mat m) = Mat $ map (map conj) m




  

