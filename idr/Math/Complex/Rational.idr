module Math.Complex.Rational

import public Math.Complex
import public Math.Rational

||| Shorthand for Complex Rational
export  
QQ : Type
QQ = Complex Rational 

||| Shorthand constructor
export
qq : Rational -> Rational -> QQ
qq = complex 
