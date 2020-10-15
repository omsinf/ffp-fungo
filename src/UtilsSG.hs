{-|
Module      : UtilsSG
Description : This module defines some utility functions
              for a more comfortable use of the ModelSG module.
-}
module UtilsSG
( emptyBoard
, b
, bp
, w
, wp
, other
, status
, stones
, showLegible
)

where

import Control.Lens ( (^.) )

import ModelSG


-- | The empty board before any move is made.
emptyBoard :: PositionSG
emptyBoard = PositionSG 0 [] [] 0 0 False Nothing Black

-- | Move: Black plays a stone at the given coordinates.
b :: Int -> Int -> MoveSG
b col row = MoveSG Black (Play (col,row))

-- | Move: Black passes.
bp :: MoveSG
bp = MoveSG Black Pass

-- | Move: White plays a stone at the given coordinates.
w :: Int -> Int -> MoveSG
w col row = MoveSG White (Play (col,row))

-- | Move: White passes.
wp :: MoveSG
wp = MoveSG White Pass

-- | Returns the opposite color.
other :: Color -> Color
other Black = White
other White = Black

-- | Reports the status of a point in a given position: free or taken by Black or White.
status :: PositionSG -> Point -> PointStatus
status position point
  | point `elem` (position^.blackStones) = Taken Black
  | point `elem` (position^.whiteStones) = Taken White
  | otherwise = Free

-- | Shorthand to access the stones of a color in a given position.
stones :: PositionSG -> Color -> [Point]
stones position Black = position^.blackStones
stones position White = position^.whiteStones


-- | Converts a move into a string that is easy to read for humans.
showLegible :: MoveSG -> String
showLegible (MoveSG color doing) = show color ++ format doing
  where
    format :: MoveGo -> String
    format Pass = " passed."
    format (Play (x,y)) = " played on (" ++ show x ++ "," ++ show y ++ ")."


-- Example - TODO OMS: take real games
pos1 = PositionSG { _movesPlayed = 10
                  , _blackStones = [(4,4),(4,5),(3,6),(3,7),(4,6),(16,16)]
                  , _whiteStones = [(3,3),(3,4),(2,5),(2,6)]
                  , _blackCaptures = 0
                  , _whiteCaptures = 0
                  , _lastPassed = False
                  , _lockedByKo = Nothing
                  , _nextToMove = Black
                  }

game1 = [b 16 4, w 4 16, b 3 4, w 17 16, b 15 16, w 15 17, b 14 17, w 16 17]
