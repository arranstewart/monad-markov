## monad-markov

A monad transformer providing a monad, `MarkovT`, in which computations have
access to state and a probabilistic transition function.
This can be used to model Markov processes.
A type class, `MonadMarkov`, is provided with one operation, "nextState", 
which transitions to the next state.

The transformer is built on top of MonadRandom
(<https://hackage.haskell.org/package/MonadRandom>).

