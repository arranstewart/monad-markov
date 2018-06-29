
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Stability    : experimental
Portability  : non-portable (multi-parameter type classes, flexible instances)

The 'MonadMarkov' class. This class abstracts over monads
which can transition from state to state probabilistically.

-}

module Control.Monad.Markov.Class (
  MonadMarkov(..) 
 )
where

import System.Random (RandomGen)
import Control.Monad.Trans.Random (RandT)
import Control.Monad.Trans.Class (MonadTrans(..))

-- | An interface to Markov process monads.
class Monad m => MonadMarkov s m where
  -- | given the current state and transition table,
  -- return a new state.
  nextState :: m s

instance (MonadMarkov s m, RandomGen g) => MonadMarkov s (RandT g m) where
  nextState = lift nextState

