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
multiply (Bra u) (Sca k) = Bra $ map (map (k *)) u
  -- what is this abonimation?
multiply (Bra u) (Bra v) = bra $ zipWith (*) (head u) (head v) 
multiply (Bra u) (Mat a) = Bra $ mmul u a 

multiply (Sca k) (Ket v) = Ket $ map (map (k *)) v 
multiply (Sca k) (Bra v) = Bra $ map (map (k *)) v
multiply (Sca k) (Sca j) = Sca $ k * j
multiply (Sca k) (Mat a) = Mat $ map (map (k *)) a

multiply (Ket u) (Bra v) = Mat $ mmul u v
multiply (Ket u) (Sca k) = Ket $ map (map (k *)) u
                                 -- XXX should be tensor product?
multiply (Ket u) (Ket v) = ket $ zipWith (*) (head $ transpose u) (head $ transpose v)
multiply (Ket u) (Mat a) = Bra $ mmul (transpose u) a -- ??? conj 

multiply (Mat a) (Mat b) = Mat $ mmul a b
multiply (Mat a) (Sca k) = Mat $ map (map (k *)) a
multiply (Mat a) (Ket v) = Ket $ mmul a v
multiply (Mat a) (Bra v) = Ket $ mmul a (transpose v) -- ??? conj

||| vanilla conjugate operator
export
conjugate : {d : Nat} -> QVect d -> QVect d
conjugate (Sca k) = Sca $ conj k  
conjugate (Bra v) = Bra $ map (map conj) v
conjugate (Ket v) = Ket $ map (map conj) v
conjugate (Mat m) = Mat $ map (map conj) m

||| transpose operator
export 
trans : {d : Nat} -> QVect d -> QVect d
trans s@(Sca _) = s
trans (Bra v) = Ket $ transpose v
trans (Ket v) = Bra $ transpose v
trans (Mat m) = Mat $ transpose m

||| conjugate transpose or adjoint operator
export
dagger : {d : Nat} -> QVect d -> QVect d
dagger = conjugate . trans

{- TODO: tensor product 
   XXXX: this is not well defined in all cases for the vector space -}
partial
{-
tensor : {d, e : Nat} -> QVect d -> QVect e -> QVect (d*e)
tensor (Bra xs) (Bra ys) = bra $ concat $ map row (head xs)
  where row x = map (* x) (head ys)
-}
tensor : {d, e : Nat} -> QVect d -> QVect e -> QVect (d*e)
tensor (Bra xs) (Bra ys) = bra $ concat $ map row (head xs)
  where row x = map (* x) (head ys)

{-                
tensor (Bra xs) (Ket ys) = ?helpme_5
tensor (Bra xs) (Sca x) = ?helpme_6
tensor (Bra xs) (Mat ys) = ?helpme_7
-}

        
-- tensor (Mat xs) (Mat ys) = Mat $ ?helpme

foobar : Vect 2 (Vect 2 Integer)  
foobar = map (map (*2)) [[1,2],[3,4]]
  
{-
tensor (Ket xs) (Bra ys) = ?helpme_4
tensor (Ket xs) (Sca x) = ?foo
tensor (Ket xs) (Mat ys) = ?helpme_10
tensor (Sca x) v = ?helpme_2
-}

{-
tensor (Mat xs) (Bra ys) = ?helpme_5
tensor (Mat xs) (Ket ys) = ?helpme_6
tensor (Mat xs) (Sca x) = ?helpme_7
-}

{- Under construction:  matrix simplification
   Look to see if we can do this Functorially -}

MatrixQQ : Nat -> Nat -> Type
MatrixQQ m n = Vect m (Vect n QQ)

VectorQQ : Nat -> Type
VectorQQ n = Vect n QQ

{-
zehfuss : MatrixQQ m n  -> MatrixQQ p q -> MatrixQQ (m * p) (n * q)
zehfuss = ?knoecker
-}

private
catrows : MatrixQQ n p -> MatrixQQ n q -> MatrixQQ n (p + q)
catrows [] [] = []
catrows (x :: xs) (y :: ys) = (x ++ y) :: (catrows xs ys)

private
catcols : MatrixQQ n p -> MatrixQQ m p -> MatrixQQ (n + m) p
catcols v w = v ++ w

private
multScalarVect : QQ -> VectorQQ n -> VectorQQ n
multScalarVect _ [] = []
multScalarVect x (y :: ys) = (x * y) :: multScalarVect x ys

private
multScalarMatrix : QQ -> MatrixQQ n m -> MatrixQQ n m
multScalarMatrix _ [] = []
multScalarMatrix x (y :: ys) = multScalarVect x y :: multScalarMatrix x ys

-- need some proof here...
private
tensorProduct' : VectorQQ n -> MatrixQQ p r -> MatrixQQ p ((S n) * r)
tensorProduct' [x] m = multScalarMatrix x m
tensorProduct' (x :: y :: ys) m = 
  (multScalarMatrix x m) `catrows` (tensorProduct' (y :: ys) m)

export
tensorProduct : MatrixQQ n m -> MatrixQQ p r -> MatrixQQ (n * p) (m * r)
tensorProduct [] _ = []
tensorProduct [v] xs = tensorProduct' v xs
tensorProduct (v :: w :: vs) xs = 
  (tensorProduct' v xs) `catcols` (tensorProduct (v::w::vs) (w :: vs) xs)
