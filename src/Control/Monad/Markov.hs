
{- |
Stability    : experimental

A Markov chain monad, built on top of 
<https://hackage.haskell.org/package/MonadRandom>.

The interface is defined by 'Control.Monad.Markov.Class.MonadMarkov'.

For example code, see the "example" directory.

-}

{-# OPTIONS_HADDOCK prune, not-home, show-extensions #-}

module Control.Monad.Markov 
  (
      MarkovT,
      MonadMarkov, 
      module Control.Monad.Markov.Internal
  )
where

import Control.Monad.Markov.Internal hiding ( MarkovT(..)  )
import Control.Monad.Markov.Internal ( MarkovT  )



