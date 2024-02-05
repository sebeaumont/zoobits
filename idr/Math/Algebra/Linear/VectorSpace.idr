module Math.Algebra.Linear.VectorSpace

import public Data.Vect

{-
-- transpose already defined in Data.Vect
transpose : { n : _ } -> Vect m (Vect n a) -> Vect n (Vect m a)
transpose [] = replicate n []
transpose (x :: xs) = zipWith (::) x $ transpose xs
-}

||| Vector dot product
export
dotproduct : Num a => { n : _ } -> Vect n a -> Vect n a -> a
dotproduct [] [] = 0
dotproduct (x :: xs) (y :: ys) = x * y + dotproduct xs ys

||| Vector matrix muliplication
export
vmul : Num a => { n, p : _ } -> Vect n a -> Vect p (Vect n a) -> Vect p a
vmul _ [] = []
vmul x (y :: ys) = (dotproduct x y) :: vmul x ys

||| Matrix-matrix multiplication
export
mmul : Num a => { m, n, p : _ } -> Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
mmul xs ys = multiply xs (transpose ys)
  where
    multiply : { m, n, p : _} -> Vect m (Vect n a) -> Vect p (Vect n a) -> Vect m (Vect p a)
    multiply [] _ = []
    multiply (x :: xs) ys = (vmul x ys) :: multiply xs ys

         
