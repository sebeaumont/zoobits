# zoobits

Various wild and domesticated creatures for writing safe eDSLs in (Idris2)

First off the bat it's indexed linear monads which can eliminate a whole
slew of bad programs with helpful messages from the typechecker.

Maybe we can do all the state machine stuff in QTT, so do we really need
indexed monads[^3]? But if its an eDSL we are after then the do notation is nice
and then there's the Free structure etc [^2]. 

But initially all this was inspired by [Qimaera](https://github.com/zamdzhiev/Qimaera)
which set the agenda[^1] and got me to learn [Idris2](https://idris2.readthedocs.io/en/latest/tutorial/index.html)

So far we have a linear version of Conor's DVDDrive example from SO.
As well as the relevant interfaces in `Idris2`.

There were some Haskell versions using linear types as well but there's
not a load of difference other than more explicit types in Idris.

Watch this space for more lolipops and zoo creatures. Not sure where 
this is going but it's cool enough that we can delegate even more 
correctness to the type checker.

Actually I'm working on some Complex number abstractions and of course 
Qimeara work already did proof of injectivity of wires and operators etc.

I'd like rationals with a few trancsendental constants thrown in and no need
for floating point except to trivially simulate things which means some
more math which may or not exist in Idris2 but proofs can be nicked/inspired
from/by beautiful Agda libraries.

There's a general idea of a eDSL for linear algebra and (quantum) computation
and machine learning lurking. 

Might go off on another tangent if I can grok this paper on Clifford
algebra based approach[^4].

The bait is taken and I'm looking as geometic algebra as a foundation now.

_________________________
Primary References:

[^1]: [Type-safe Quantum Programming in Idris](https://arxiv.org/abs/2111.10867)
[^2]: [The Kleisli Arrows of Outrageous Fortune](https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf)
[^3]: [Quantitive Type Theory in Practice](https://arxiv.org/abs/2104.00480)
[^4]: [Quantum Computing Based on Complex Clifford Algebras](https://arxiv.org/abs/2201.02246)
