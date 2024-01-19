module Complex

-- Hopefully someone has done this right somewhere...
record Complex f where
  constructor C
  real : f
  imag : f

conj : Neg ty => Complex ty -> Complex ty  
conj z = C z.real (negate z.imag)

-- metric for complex numbers
{-
failing 
  abs2 : Num ty => Complex ty -> Complex ty
  abs2 z = z * conj z
-}

-- simple version
abs2 : Num ty => Complex ty -> ty
abs2 z = z.real * z.real + z.imag * z.imag

implementation Eq ty => Eq (Complex ty) where
  z == w = real z == real w && imag z == imag w
   
implementation (Neg ty, Num ty) => Num (Complex ty) where
  z * w = C (z.real*w.real - z.imag*w.imag) (z.real*w.imag + z.imag*w.imag)
  z + w = C (z.real + w.real) (z.imag + w.imag)
  fromInteger i = C (fromInteger i) 0 

implementation Neg ty => Neg (Complex ty) where
  negate (C r i) = C (-r) (-i)
  z - w = C (z.real - w.real) (z.imag - w.imag)

implementation (Fractional ty, Neg ty, Num ty) => Fractional (Complex ty) where
  recip z = 
    -- do simple arithetic so we don't have a circular definition,
    -- thus we rely on abs2 z being real
    let (C r i) = conj z 
        s = abs2 z 
    in C (r/s) (i/s)
  z / w = z * recip w

-- Mixed arithmetic scale from left and right.?

scale : Num a => Complex a -> a -> Complex a
scale (C r i) x = C (r*x) (i*x)

(*) : Num a => Complex a -> a -> Complex a
(*) = scale

implementation (Ord ty, Num ty, Show ty) => Show (Complex ty) where
  show (C r i) = "(" ++ show r ++ sign i ++ show i ++ "i)" where
    sign : ty -> String
    sign i = if i > 0 then "+" else ""

record Phasor f where
  constructor P
  modulus : f
  phase: f

-- | TODO make this more general given needs to be more generic!
phasor : Complex Double -> Phasor Double
phasor z@(C x y) = P (sqrt $ abs2 z) (atan $ y / x)

-- Testing --

c0 : Complex Double
c0 = (C 0.0 0.0)

c1 : Complex Double
c1 = (C 1.0 0.0)

c2 : Complex Double
c2 = (C 2.0 1.0)

c1i : Complex Double
c1i = (C 1.0 1.0)

c3i : Complex Double
c3i = (C 3.0 1.0)

pipi : Complex Double
pipi = c1i * pi
