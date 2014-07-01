{-# LANGUAGE TemplateHaskell #-}


module Types where


import Control.Lens
import Text.Parsec.Pos (SourcePos)

import qualified Data.Map


-- (x, y)
data Point = Point {
    _x :: Double,
    _y :: Double }
  deriving (Eq, Ord, Show)

$(makeLenses ''Point)


-- Ax + By + C = 0
data Line = Line {
    _a :: Double,
    _b :: Double,
    _c :: Double }
  deriving (Eq, Ord, Show)

$(makeLenses ''Line)


data Command = O1 { _p1 :: Point, _p2 :: Point }
             | O2 { _p1 :: Point, _p2 :: Point } 
             | O3 { _l1 :: Line, _l2 :: Line }
             | O4 { _l :: Line, _p :: Point }
             | O5 { _l :: Line, _p1 :: Point, _p2 :: Point }
             | O6 { _l1 :: Line, _p1 :: Point, _l2 :: Line, _p2 :: Point }
             | O7 { _l1 :: Line, _l2 :: Line, _p :: Point }
  deriving (Eq, Ord, Show)

$(makeLenses ''Command)


-- Parser stuff
data ParserState = ParserState {
    _psPoints :: Data.Map.Map String Point,
    _psLines :: Data.Map.Map String Line,
    _psCommands :: [Command] }
  deriving (Eq, Ord, Show)

emptyState :: ParserState
emptyState = ParserState {
    _psPoints = Data.Map.empty,
    _psLines = Data.Map.empty,
    _psCommands = [] }
  
$(makeLenses ''ParserState)


data OCMProgram = OCMProgram {
    _pCommands :: [Command] }
  deriving (Eq, Ord, Show)

emptyProgram :: OCMProgram
emptyProgram = OCMProgram []

$(makeLenses ''OCMProgram)


-- Tokenizer stuff
type TokenPos = (Token, String, SourcePos)

data Token = TokenLeftParen
           | TokenRightParen

           | TokenDouble Double
           | TokenIdentifier String

           | TokenDefPoint
           | TokenDefLine

           | TokenO1
           | TokenO2
           | TokenO3
           | TokenO4
           | TokenO5
           | TokenO6
           | TokenO7
  deriving (Eq, Ord, Show)
