module MiniForth.Parser
    ( programParser
    ) where

import Text.Parsec.Number
import Text.Parsec

import MiniForth.Types

programParser :: Parsec String () [Token]
programParser = do
    ts <- tokenParser `sepEndBy` spaces
    eof
    return ts

tokenParser :: Parsec String () Token
tokenParser = wordParser <|> numberParser <|> defParser where
    specials = oneOf "!#$%&|*+-/<=>?@^_~."

    wordParser = fmap Word wordp

    wordp = do
        c <- letter <|> specials
        cs <- many (letter <|> specials <|> digit <|> char ':')
        return (c:cs)

    numberParser = fmap Number (floating3 True)

    defParser = do
        _ <- char ':'
        spaces
        w <- wordp
        spaces
        ws <- tokenParser `sepEndBy` spaces
        _ <- char ';'
        return (Def w ws)
