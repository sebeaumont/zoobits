||| Rational numbers 
module Math.Rational

%default total

||| Rationals based on Integers
export
record Rational where
  constructor Q
  numerator   : Integer
  denominator : Integer  -- Must be positive

-- Since we know that b is +ve then this is safe.
--  At least we MUST KNOW THAT b is not 0! 
private 
gcd : Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b $ assert_smaller b (a `mod` b)

-- This is the `unsafe` version we use internally with the assurance that @b@ is
-- always positive as the public constructor for Rational requires a proof.

private
normalize : Integer -> Integer -> (Integer, Integer) 
normalize a b = let d = gcd a b in (div a d, div b d)

||| Equality theorem for positive integers

public export  
isPositive : Integer -> Type
isPositive i = i > 0 = True

||| Make a rational number.
|||  You need a proof that the denominator is positve. 

export
rational : (a : Integer) -> (b : Integer) -> {auto p : isPositive b} -> Rational
rational _ 0 = Q 0 1
rational a b = let (n, d) = normalize a b in Q n d

export
infixl 9 //

||| Infix operator way of writing a ratio
|||  You need a proof that the denominator is positve. 
export
(//) : (a : Integer) -> (b : Integer) -> {auto p : isPositive b} -> Rational
(//) = rational

export
implementation Show Rational where
  show q = (show q.numerator) ++ "/" ++ (show q.denominator)

export        
implementation Eq Rational where
  p == q = p.numerator == q.numerator 
           && p.denominator == q.denominator 

export
implementation Num Rational where
  p * q = 
    let num = p.numerator * q.numerator
        den = p.denominator * q.denominator
        (n, d) = normalize num den
    in Q n d
    
  p + q = 
    let quot = p.denominator * q.denominator
        numr = p.numerator * q.denominator + q.numerator * p.denominator
        (n, d) = normalize numr quot
    in Q n d
  
  fromInteger q = rational q 1

export
implementation Neg Rational where
  negate q  = Q (negate q.numerator) q.denominator
  q - r = q + (negate r)

signum : Integer -> Integer
signum i = if i < 0 then -1 else 1

export
implementation Fractional Rational where
  recip q = Q (q.denominator * (signum q.numerator)) (abs q.numerator) 
  q / r = q * recip r

export 
implementation Ord Rational where
  compare q r = 
    let a = q.numerator * r.denominator 
        b = r.numerator * q.denominator
    in compare a b
    
export
rationalToDouble : Rational -> Double
rationalToDouble (Q p q) = (fromInteger p) / (fromInteger q)

-- We add a few irrationals and transcendental numbers to the party
-- These approximations should be ok pro tem.
-- TODO provide the series/order and return the residual (or specify it) 
-- in a separate module.
public export
data Constant = Pi | E | R2 | R3

export
implementation Show Constant where
  show Pi = "π"
  show E = "e"
  show R2 = "√2"
  show R3 = "√3"
  
value : Constant -> Rational
value Pi = 80143857//25510582
value E = 28245729//10391023
value R2 = 3880899//2744210
value R3 = 262087//151316

export
prefix 8 #.
export
(#.) : Constant -> Rational
(#.) = value
