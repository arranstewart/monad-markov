

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances   #-}

{- |
Stability    : experimental
Portability  : non-portable (multi-parameter type classes, generalized newtype deriving, flexible instances)

A Markov chain monad, built on top of 'MonadRandom'.

The interface is defined by 'Control.Monad.Markov.Class.MonadMarkov'.

For example code, see the "example" directory.

-}

{-# OPTIONS_HADDOCK prune, not-home, show-extensions #-}

module Control.Monad.Markov.Internal 
  (
       MarkovT(..)
    ,  module Control.Monad.Markov.Internal
--    , Rand(..)
--    , RandT(..)
--    , mkStdGen
--    , RandomGen(..)
    , MonadRandom(..)
--    , StdGen
    , module Control.Monad.Markov.Class
  )
where

import System.Random
import Control.Monad.Random
import Data.Ratio
import Control.Monad.State 
import Control.Monad.Reader
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Markov.Class
import Control.Monad


-- | A monad transformer which adds access to a state and a
-- probabilistic transition function to an existing
-- monad.
--
-- Parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of
-- the second.
newtype MarkovT s m a = MarkovT {
  unMarkovT :: StateT s (ReaderT (s -> [(s, Rational)]) m) a
  }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadRandom, MonadInterleave)

-- | a transition function, from a state, to a weighted
-- list of states. The total weight of states must not be 0.
type TransTable a = (a -> [(a, Rational)]) 


-- | A basic Markov monad
type Markov s a = MarkovT s Identity a

-- | Markov monad transformer, laid over the 'RandT' random 
-- value monad.
type MarkovRT s g m = MarkovT s (RandT g m)

-- | Basic markov monad laid over the 'RandT' random value monad.
type MarkovR s g = MarkovT s (Rand g)

-- | Markov monad transformer, using the standard random generator
type MarkovStdT s m = MarkovRT s StdGen m


-- | Basic Markov monad, using the standard random generator 'StdGen'
type MarkovStd s = MarkovR s StdGen 

-- | @withMarkovT f m@ executes action @m@ on a state modified by applying
-- @f@.
withMarkovT :: (s -> s) -> MarkovT s m a -> MarkovT s m a
withMarkovT f (MarkovT m) = MarkovT $ withStateT f m 

-- | Evaluate a Markov chain computation with a given initial state
-- and transition table, 
-- and return the final value, discarding the final state.
evalMarkovT
  :: Monad m => MarkovT s m a -- ^computation to execute
             -> s             -- ^initial state
             -> TransTable s  -- ^transition function to use
             -> m a
evalMarkovT (MarkovT f)   = runReaderT . evalStateT f

-- | Evaluate a Markov chain computation with a given initial state
-- and transition table, 
-- and return the final value, discarding the final state.
evalMarkov :: Markov s a    -- ^computation to execute
           -> s             -- ^initial state
           -> TransTable s  -- ^transition function to use
           -> a
evalMarkov m s = runIdentity . evalMarkovT m s 


-- | Unwrap a Markov monad computation as a function.
runMarkovT :: MarkovT s m a -- ^computation to execute
           -> s             -- ^initial state
           -> TransTable s  -- ^transition function to use
           -> m (a, s)
runMarkovT (MarkovT f) s  = runReaderT (runStateT f s)

-- | Unwrap a 'Markov' monad computation as a function.
runMarkov :: Markov s a   -- ^computation to execute
          -> s            -- ^initial state
          -> TransTable s -- ^transition function to use
          -> (a, s)
runMarkov m s = runIdentity . runMarkovT m s 


-- | Run a Markov computation using the generator @g@,
-- returning the result, the updated generator, and the
-- the final state.
runMarkovRT :: Functor m 
            => MarkovRT s g m a -- ^computation to execute 
            -> g                -- ^generator to use
            -> s                -- ^initial state
            -> TransTable s     -- ^transition function to use
            -> m (a, g, s)
runMarkovRT f g c t = (\((a,s),g) -> (a,g,s))
                            <$> runRandT (runMarkovT f c t) g

-- | Run a Markov computation using the generator @g@,
-- returning the result, the updated generator, and the
-- the final state.
runMarkovR
  :: MarkovR s g a          -- ^computation to execute 
            -> g            -- ^generator to use
            -> s            -- ^initial state
            -> TransTable s -- ^transition function to use
            -> (a, g, s)
runMarkovR f c t = runIdentity . runMarkovRT f c t


-- | Run a Markov computation 
-- using the random number
-- generator used by 'System.Random.getStdRandom',
-- initialised with the seed @seed@,
-- returning the result, the updated generator, and the
-- the final state.
runMarkovStdT :: Functor m
              => MarkovStdT s m a -- ^computation to execute
              -> Int              -- ^ seed for generator
              -> s                -- ^initial state
              -> TransTable s     -- ^transition function to use
              -> m (a, StdGen, s)
runMarkovStdT f seed  = runMarkovRT f (mkStdGen seed) 

-- | Run a Markov computation 
-- using the random number
-- generator used by 'System.Random.getStdRandom',
-- initialised with the seed @seed@,
-- returning the result, the updated generator, and the
-- the final state.
runMarkovStd :: MarkovStd s a -- ^computation to execute
             -> Int           -- ^ seed for generator
             -> s             -- ^initial state
             -> TransTable s  -- ^transition function to use
             -> (a, StdGen, s)
runMarkovStd f seed s = runIdentity . runMarkovStdT f seed s



-- | Evaluate a 'MarkovRT' computation using the random generator @g@. 
evalMarkovRT :: Monad m 
             => MarkovRT s g m a -- ^computation to execute 
             -> g                -- ^generator to use
             -> s                -- ^initial state
             -> TransTable s     -- ^transition function to use
             -> m a
evalMarkovRT f g c t = evalRandT (evalMarkovT f c t) g

-- | Evaluate a 'MarkovR' computation using the random generator @g@. 
evalMarkovR :: MarkovR s g a -- ^computation to execute 
             -> g            -- ^generator to use
             -> s            -- ^initial state
             -> TransTable s -- ^transition function to use
             -> a
evalMarkovR f g c  = runIdentity . evalMarkovRT f g c 

-- | Evaluate a 'MarkovRT' computation using the standard random generator,
-- initialized with @seed@.
evalMarkovStdT :: Monad m 
             => MarkovStdT s m a -- ^computation to execute 
             -> Int              -- ^ seed for generator
             -> s                -- ^initial state
             -> TransTable s     -- ^transition function to use
             -> m a
evalMarkovStdT f seed  = evalMarkovRT f (mkStdGen seed)

-- | Evaluate a 'MarkovRT' computation using the standard random generator,
-- initialized with @seed@.
evalMarkovStd :: MarkovStd s a -- ^computation to execute 
             -> Int            -- ^ seed for generator
             -> s              -- ^initial state
             -> TransTable s   -- ^transition function to use
             -> a
evalMarkovStd f c t = runIdentity . evalMarkovStdT f c t 

-- | Evaluate a Markov computation in the 'IO' monad, using
-- the standard random generator supplied by 'getStdGen'.
evalMarkovIO :: MarkovStdT s IO a -- ^computation to execute 
             -> s                 -- ^initial state
             -> TransTable s      -- ^transition function to use
             -> IO a
evalMarkovIO f c t = 
  getStdGen >>= (\g -> evalMarkovRT f g c t)


-- requires undecidable instances
--instance (MonadSplit g m) => MonadSplit g (MarkovT m) where
--  getSplit = lift getSplit

instance MonadRandom m => MonadMarkov s (MarkovT s m) where
  nextState = MarkovT $ do
    table <- ask
    currState <- get
    nextState <- fromList (table currState)
    put nextState
    return currState

