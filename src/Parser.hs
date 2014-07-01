module Parser where


import Control.Lens

import Text.Printf (printf)

import qualified Data.Map
import qualified Text.Parsec as Parsec

import Types


-- Parser alias
type Parser a = Parsec.Parsec [TokenPos] ParserState a


(<?>) :: String -> Parser a -> Parser a
(<?>) = flip (Parsec.<?>)

-- Helpers
posOf :: (a, b, c) -> c
posOf (_, _, pos) = pos

match :: Token -> a -> Parser a
match tok parser = do
    _ <- Parsec.token show posOf check
    return parser
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
p_defpt = "Point definition" <?> p_
  where p_ = parens $ do
                match TokenDefPoint ()
                ident <- matchIdentifier
                pt1 <- matchDouble
                pt2 <- matchDouble
                let point = Point pt1 pt2
                Parsec.modifyState (update ident point)
        update ident point state = state & psPoints %~ Data.Map.insert ident point

p_defln :: Parser ()
p_defln = "Line definition" <?> p_
  where p_ = parens $ do
                match TokenDefLine ()
                ident <- matchIdentifier
                ax <- matchDouble
                by <- matchDouble
                c_ <- matchDouble
                let line = Line ax by c_
                Parsec.modifyState (update ident line)
        update ident line state = state & psLines %~ Data.Map.insert ident line

p_o1 :: Parser ()
p_o1 = "Axiom O1" <?> p_
  where p_ = parens $ do
              match TokenO1 ()
              p1i <- matchIdentifier
              p2i <- matchIdentifier

              state <- Parsec.getState

              pt1 <- case Data.Map.lookup p1i $ state ^. psPoints of
                  Nothing -> fail $ printf "O1 could not find first point %s" p1i
                  Just pt -> return pt

              pt2 <- case Data.Map.lookup p2i $ state ^. psPoints of
                  Nothing -> fail $ printf "O1 could not find second point %s" p2i
                  Just pt -> return pt

              let command = O1 pt1 pt2
              Parsec.modifyState (update command)
        update command state = state & psCommands %~ (:) command


p_ocm_program :: Parser OCMProgram
p_ocm_program = "Full OCM program" <?> do
    _ <- Parsec.many $ Parsec.choice $ map Parsec.try [p_defpt, p_defln, p_o1]
    Parsec.eof
    state <- Parsec.getState
    return $ OCMProgram $ state ^. psCommands

-- External parser starting point
parseOCM :: [TokenPos] -> Either Parsec.ParseError OCMProgram
parseOCM input = Parsec.runParser p_ocm_program emptyState "(unknown)" input
