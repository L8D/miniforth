module Main
    ( main
    ) where

import System.Console.Readline
import Control.Monad.Except
import Control.Monad.State
import Text.Parsec

import qualified Data.Map as M

import MiniForth

main :: IO ()
main = withWorld (World [] M.empty) (load >> repl)

withWorld :: World -> VM () -> IO ()
withWorld w r = runStateT (runExceptT (runVM r)) w >>= go where
    go (Left ProgramExit, _) = return ()
    go (Left e, s)           = liftIO (print e) >> withWorld s repl
    go (Right (), _)         = return ()

repl :: VM ()
repl = liftIO (readline "% ") >>= maybe (liftIO $ putStrLn "") go where
    go line = do
        liftIO $ addHistory line
        case parse programParser "" line of
            Left e   -> liftIO $ print e
            Right xs -> run xs
        repl
