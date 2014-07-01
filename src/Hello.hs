module Main where


import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec


p_hello :: Parsec.Parser String
p_hello = Parsec.string "Hello"

p_world :: Parsec.Parser String
p_world = Parsec.string "World"


p_hello_world :: Parsec.Parser String
p_hello_world = do
    str1 <- p_hello
    Parsec.char ','
    Parsec.spaces
    str2 <- p_world
    Parsec.char '!'
    Parsec.spaces
    Parsec.eof
    return $ str1 ++ " " ++ str2


-- And now, the actual parser
parse :: Parsec.SourceName -> String -> Either Parsec.ParseError String
parse = Parsec.runParser p_hello_world ()

-- Actual program
main :: IO ()
main = do
    contents <- getContents

    case parse "stdin" contents of
        Left pe -> error $ show pe

        Right str -> do
            putStrLn $ "Parsed: " ++ str
            return ()
