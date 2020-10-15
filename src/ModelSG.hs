{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : ModelSG
Description : This module contains the data model for a simple Go application.
-}
module ModelSG where

import Control.Lens ( makeLenses )



-- | Go is played by two opponents: Black and White.
data Color = Black | White deriving (Eq, Show)


-- | A point represents the coordinates of an intersection at the Go board's grid.
-- We typically interprete the first value as "column", the second as "row".
type Point = (Int,Int)


-- | Each intersection can be free or taken by a stone of either Black or White.
data PointStatus = Taken Color | Free deriving (Eq)


-- | A position as it arises at any moment of the game is represented by several aspects.
-- In sum, they contain all information necessary to continue the game.
data PositionSG =
  PositionSG  { _movesPlayed   :: Int         -- ^ The number of moves played up to this position.
              , _blackStones   :: [Point]     -- ^ The points where black stones are placed.
              , _whiteStones   :: [Point]     -- ^ The points where white stones are placed.
              , _blackCaptures :: Int         -- ^ The number of stones that Black has captured.
              , _whiteCaptures :: Int         -- ^ The number of stones that White has caputred.
              , _lastPassed    :: Bool        -- ^ Flag indicating whether the last move was 'pass'.
              , _lockedByKo    :: Maybe Point -- ^ If a Ko is played, the point which is locked for the next move.
              , _nextToMove    :: Color       -- ^ The color that is to move next.
}
makeLenses ''PositionSG

instance Show PositionSG where
  show pos = "\nBlack: " ++ show (_blackStones pos) ++ ", captures: " ++ show (_blackCaptures pos)
          ++ "\nWhite: " ++ show (_whiteStones pos) ++ ", captures: " ++ show (_whiteCaptures pos)
          ++ "\nMoves played: " ++ show (_movesPlayed pos) ++ ", last passed: " ++ show (_lastPassed pos)
          ++ "\nTo move: " ++ show (_nextToMove pos) ++ ", Ko lock: " ++ show (_lockedByKo pos)
          ++ "\n"


-- | A move in Go consists of either passing (doing nothing) or playing a stone at a point.
data MoveGo = Pass | Play Point deriving (Eq)

instance Show MoveGo where
  show Pass = "[]"
  show (Play (x,y)) = "[" ++ show x ++ "," ++ show y ++ "]"


-- | A move's representation in this module includes the playing color and the move itself.
data MoveSG = MoveSG { _color :: Color
                     , _doing :: MoveGo } deriving (Eq)
makeLenses ''MoveSG

instance Show MoveSG where
  show (MoveSG Black doing) = 'B' : show doing
  show (MoveSG White doing) = 'W' : show doing


-- | A list of moves makes a game. The head of the list contains the first move,
-- each following move is added at the end of the list.
type GameSG = [MoveSG]
