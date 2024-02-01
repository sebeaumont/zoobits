module Math.Complex

||| Generic complex number arithmetic in cartesian representation. 
export
record Complex k where
  constructor C
  real : k
  imag : k

||| Make a complex number with real and imaginary parts
export
complex : k -> k -> Complex k
complex = C

||| Real part of complex number
export 
re : Complex k -> k
re = real

||| Imaginary part of complex number
export 
im : Complex k -> k
im = imag

||| Complex conjugage
export
conj : Neg ty => Complex ty -> Complex ty  
conj z = C z.real (negate z.imag)

||| Squared absolute (scalar) value of complex number (modulus)
export
abs2 : Num ty => Complex ty -> ty
abs2 z = z.real * z.real + z.imag * z.imag

export
implementation Eq ty => Eq (Complex ty) where
  z == w = real z == real w && imag z == imag w

export   
implementation (Neg ty, Num ty) => Num (Complex ty) where
  z * w = C (z.real*w.real - z.imag*w.imag) (z.real*w.imag + z.imag*w.imag)
  z + w = C (z.real + w.real) (z.imag + w.imag)
  fromInteger i = C (fromInteger i) 0 
  
export
implementation Neg ty => Neg (Complex ty) where
  negate (C r i) = C (-r) (-i)
  z - w = C (z.real - w.real) (z.imag - w.imag)

export
implementation (Fractional ty, Neg ty, Num ty) => Fractional (Complex ty) where
  recip z = 
    -- do simple arithetic so we don't have a circular definition,
    -- thus we rely on abs2 z being real
    let (C r i) = conj z 
        s = abs2 z 
    in C (r/s) (i/s)
  z / w = z * recip w

||| Scale complex number from left
export
scale : Num a => a -> Complex a -> Complex a
scale x (C r i) = C (r*x) (i*x)

||| scale complex number from left
export
(*) : Num a => a -> Complex a -> Complex a
(*) = scale

export
implementation (Ord ty, Num ty, Show ty) => Show (Complex ty) where
  show (C r i) = "(" ++ show r ++ sign i ++ show i ++ "i)" where
    sign : ty -> String
    sign i = if i > 0 then "+" else ""

