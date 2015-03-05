module MiniForth.Engine
    ( pop
    , push
    , define
    , run
    , word
    ) where

import Control.Monad.Except
import Control.Lens

import MiniForth.Types

pop :: VM Double
pop = use stack >>= go where
    go []     = throwError StackUnderflow
    go (x:xs) = do
        stack .= xs
        return x

push :: Double -> VM ()
push x = stack %= (x:)

define :: String -> VM () -> VM ()
define name block = dict . at name ?= block

run :: [Token] -> VM ()
run []     = return ()
run (x:xs) = case x of
    Word w   -> word w >> run xs
    Number n -> push n >> run xs
    Def w b  -> define w (run b) >> run xs

word :: String -> VM ()
word w = use (dict . at w) >>= maybe (throwError (UndefinedWord w)) id
