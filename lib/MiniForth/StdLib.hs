module MiniForth.StdLib
    ( load
    ) where

import Control.Monad.Random
import Control.Monad.Except
import Control.Applicative
import Control.Lens
import Data.Fixed           (mod', div')

import MiniForth.Engine
import MiniForth.Types

load :: VM ()
load = do
    define "."     $ pop >>= output
    define "rand"  $ getRandomR (0, 1) >>= push
    define "pick"  $ popI >>= pick
    define "roll"  $ popI >>= roll
    define "rot"   $ roll 2
    define "drop"  $ pop >> return ()
    define "over"  $ pick 1
    define "swap"  $ roll 1
    define "dup"   $ pick 0
    define "2dup"  $ twodup
    define "+"     $ twomap (+)
    define "-"     $ twomap (-)
    define "*"     $ twomap (*)
    define "/"     $ twomap (/)
    define "%"     $ twomap mod'
    define "="     $ bwomap (==)
    define ">"     $ bwomap (>)
    define "<"     $ bwomap (<)
    define ">="    $ bwomap (>=)
    define "<="    $ bwomap (<=)
    define "min"   $ twomap min
    define "max"   $ twomap max
    define "and"   $ twomap and'
    define "or"    $ twomap or'
    define "atan2" $ twomap atan2
    define "^"     $ liftA2 (flip (^^)) popI pop >>= push
    define "sqrt"  $ topmap sqrt
    define "sin"   $ topmap sin
    define "cos"   $ topmap sin
    define "tan"   $ topmap tan
    define "neg"   $ topmap negate
    define "log"   $ topmap log
    define "ceil"  $ iopmap ceiling
    define "floor" $ iopmap floor
    define "round" $ iopmap round

popI :: VM Int
popI = round <$> pop

pick :: Int -> VM ()
pick n = use stack >>= go where
    go = maybe (throwError (AddressOutOfBounds n)) push . ind n
    ind _ []     = Nothing
    ind 0 (x:_)  = Just x
    ind i (_:xs) = ind (pred i) xs

roll :: Int -> VM ()
roll n = uses stack (splitAt n) >>= go where
    go (xs, y:ys) = stack .= (y:xs ++ ys)
    go (_,  [])   = throwError (AddressOutOfBounds n)

twodup :: VM ()
twodup = pick 2 >> pick 2

output :: Double -> VM ()
output n = liftIO $ putStrLn s where
    s = if n `mod'` 1 == 0 then show (n `div'` 1 :: Int) else show n

twomap :: (Double -> Double -> Double) -> VM ()
twomap f = liftA2 (flip f) pop pop >>= push

topmap :: (Double -> Double) -> VM ()
topmap f = f <$> pop >>= push

iopmap :: (Double -> Int) -> VM ()
iopmap f = f <$> pop >>= push . fromIntegral

and' :: Double -> Double -> Double
and' 0 _ = 0
and' _ 0 = 0
and' _ _ = 1

or' :: Double -> Double -> Double
or' 0 0 = 0
or' _ _ = 1

bwomap :: (Double -> Double -> Bool) -> VM ()
bwomap f = liftA2 (\x y -> fromB $ f y x) pop pop >>= push where
    fromB True = 1
    fromB False = 0
