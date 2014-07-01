module Tokenizer (tokenize) where


import Control.Applicative

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec

import Types


chars :: [ (Token, Char) ]
chars = [ (TokenLeftParen, '('),
          (TokenRightParen, ')') ]

keywords :: [ (Token, String) ]
keywords = [ (TokenO1, "o1"),
             (TokenO2, "o2"),
             (TokenO3, "o3"),
             (TokenO4, "o4"),
             (TokenO5, "o5"),
             (TokenO6, "o6"),
             (TokenO7, "o7"),

             (TokenDefPoint, "defpt"),
             (TokenDefLine, "defln") ]


-- Generate a Parsec token parser / lexer
lexer :: Parsec.TokenParser ()
lexer = Parsec.makeTokenParser Parsec.haskellDef {
    Parsec.commentStart = "/*",
    Parsec.commentEnd = "",
    Parsec.commentLine = "",
    Parsec.nestedComments = False,
    Parsec.reservedNames = map snd keywords,
    Parsec.reservedOpNames = [] }

p_chars :: Parsec.Parser TokenPos
p_chars = Parsec.choice $ map convert chars
  where convert (tok, char) = p_char tok char
        p_char token char = do
            pos <- Parsec.getPosition
            _ <- Parsec.char char
            return (token, [char], pos)

p_reserved_keywords :: Parsec.Parser TokenPos
p_reserved_keywords = Parsec.choice $ map convert keywords
  where convert (tok, word) = p_reserved tok word
        p_reserved token str = do
            pos <- Parsec.getPosition
            _ <- Parsec.reserved lexer str
            return (token, str, pos)

p_double :: Parsec.Parser TokenPos
p_double = do
    pos <- Parsec.getPosition
    float <- Parsec.float lexer
    return (TokenDouble float, show float, pos)

p_identifier :: Parsec.Parser TokenPos
p_identifier = do
    pos <- Parsec.getPosition
    ident <- Parsec.identifier lexer
    return (TokenIdentifier ident, ident, pos)

p_tokens :: Parsec.Parser [TokenPos]
p_tokens = do
    Parsec.spaces
    tokens <- Parsec.sepEndBy1 p_token $ Parsec.choice [Parsec.spaces, Parsec.eof]
    return tokens
  where p_token = Parsec.choice [ p_chars, p_reserved_keywords, p_double, p_identifier ]


-- And now, the actual parser
tokenize :: Parsec.SourceName -> String -> Either Parsec.ParseError [TokenPos]
tokenize = Parsec.runParser p_tokens ()
