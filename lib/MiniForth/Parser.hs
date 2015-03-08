module MiniForth.Parser
    ( programParser
    ) where

import Text.Parsec.Number
import Text.Parsec
import Data.Maybe         (isJust)
import Data.Char          (toLower)

import MiniForth.Types

programParser :: Parsec String () [Token]
programParser = do
    spaces
    ts <- tokenParser `sepEndBy` spaces
    eof
    return ts

tokenParser :: Parsec String () Token
tokenParser = wordParser <|> numberParser <|> defParser where
    specials = oneOf "!#$%&|*+/<=>?@^,_~."

    wordParser = fmap Word wordp

    wordp = do
        c <- letter <|> specials
        cs <- many (letter <|> specials <|> digit <|> oneOf ":-")
        return (map toLower (c:cs))

    numberParser = do
        neg <- fmap isJust $ optionMaybe $ char '-'
        n <- floating3 True
        return $ Number $ if neg then negate n else n

    defParser = do
        _ <- char ':'
        spaces
        w <- wordp
        spaces
        ws <- tokenParser `sepEndBy` spaces
        _ <- char ';'
        return (Def w ws)
