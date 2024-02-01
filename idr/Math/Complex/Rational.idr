module Math.Complex.Rational

import public Math.Complex
import public Math.Rational

{- importing this seems to confuse Idris more than the conveience -}
||| Shorthand for Complex Rational
export  
QQ : Type
QQ = Complex Rational 

||| Shorthand constructor
export
qq : Rational -> Rational -> QQ
qq = complex 
