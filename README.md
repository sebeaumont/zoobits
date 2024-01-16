# Dromas - It's a Runner!

Well first off its indexed linear monads for fun and profit -- mainly to see
how far we can push linear and dependant types in Haskell using Idris
as the foil. The main gripes and gotchas so far:

1. Idris needs lolipops the (1_ : Foo) -> Bar instead of Foo -o Bar is
   - Apparently there is -@ in Data.Linear.Notation!
  
2. Haskell has not got enough dependant types to assert injective sets
   and the like - it may never have that.
   
TODO: References
