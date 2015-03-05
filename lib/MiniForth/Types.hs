{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module MiniForth.Types
    ( VM(..)
    , Token(..)
    , World(..)
    , Err(..)
    , stack
    , dict
    ) where

import Control.Monad.Random
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Control.Lens

import qualified Data.Map as M

data Token
    = Word String
    | Number Double
    | Def String [Token]

newtype VM a = VM { runVM :: ExceptT Err (StateT World IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState World
             , MonadError Err
             , MonadRandom
             , MonadIO
             )

data Err
    = StackUnderflow
    | UndefinedWord String
    | AddressOutOfBounds Int
    | ProgramExit

instance Show Err where
    show StackUnderflow         = "stack underflow"
    show (UndefinedWord w)      = "undefined word \"" ++ w ++ "\""
    show (AddressOutOfBounds i) = "address " ++ show i ++ " out of bounds"
    show ProgramExit            = "exit"

data World = World
    { _stack :: [Double]
    , _dict  :: M.Map String (VM ())
    }

makeLenses ''World
