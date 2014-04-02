module Parser where


import Control.Lens

import Text.Printf (printf)

import qualified Data.Map
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Parsec

import Types


-- Parser alias
type Parser a = Parsec.Parsec [TokenPos] ParserState a


-- Helpers
posOf :: (a, b, c) -> c
posOf (_, _, pos) = pos

match :: Token -> a -> Parser a
match tok a = do
    _ <- Parsec.token show posOf check
    return a
  where check (t, _, _) | t == tok = Just tok
        check _ = Nothing

matchIdentifier :: Parser String
matchIdentifier = Parsec.token show posOf check
  where check (TokenIdentifier ti, _, _) = Just ti
        check _ = Nothing

matchDouble :: Parser Double
matchDouble = Parsec.token show posOf check
  where check (TokenDouble td, _, _) = Just td
        check _ = Nothing

parens :: Parser a -> Parser a
parens = Parsec.between (match TokenLeftParen ()) (match TokenRightParen ())


-- Parsers
p_defpt :: Parser ()
p_defpt = parens $ do
    match TokenDefPoint ()
    ident <- matchIdentifier
    p1 <- matchDouble
    p2 <- matchDouble
    let point = Point p1 p2
    Parsec.modifyState $ \state -> state & psPoints %~ Data.Map.insert ident point

p_defln :: Parser ()
p_defln = parens $ do
    match TokenDefLine ()
    ident <- matchIdentifier
    a <- matchDouble
    b <- matchDouble
    c <- matchDouble
    let line = Line a b c
    Parsec.modifyState $ \state -> state & psLines %~ Data.Map.insert ident line

p_o1 :: Parser ()
p_o1 = parens $ do
    match TokenO1 ()
    p1i <- matchIdentifier
    p2i <- matchIdentifier

    state <- Parsec.getState

    case (Data.Map.lookup p1i $ state ^. psPoints, 
          Data.Map.lookup p2i $ state ^. psPoints) of
        (Nothing, Nothing) -> fail $ printf "O1 could not find either %s or %s points" p1i p2i
        (_,       Nothing) -> fail $ printf "O2 could not find first point %s" p1i
        (Nothing, _      ) -> fail $ printf "O2 could not find second point %s" p2i
        (Just p1, Just p2) -> do
            let command = O1 p1 p2
            Parsec.modifyState $ \state -> state & psCommands %~ (:) command

p_ocm_program :: Parser OCMProgram
p_ocm_program = do
    Parsec.many $ Parsec.choice $ map Parsec.try [p_defpt, p_defln, p_o1]
    Parsec.eof
    state <- Parsec.getState
    return $ OCMProgram $ state ^. psCommands

-- External parser starting point
parseOCM :: [TokenPos] -> Either Parsec.ParseError OCMProgram
parseOCM input = Parsec.runParser p_ocm_program emptyState "(unknown)" input
