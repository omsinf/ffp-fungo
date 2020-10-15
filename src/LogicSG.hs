{-|
Module      : LogicSG
Description : This module implementys key functionality to play Go
              using the data structure defined in the ModelSG module.

              You can play easily like this:
              go (b 16 16) // w 4 4 // b 16 4 // w 4 16
-}
module LogicSG
( play
, go
, (//)
, calculatePosition
)

where

import Control.Lens ( (&), (^.), over, set )
import Data.List ( (\\), nub, union )

import ModelSG
import ParseSG
import UtilsSG


-- | Takes the next move in a given game and returns either the new game,
-- it the move is legal, or a notification about why the move is illegal.
play :: MoveSG -> GameSG -> Either String GameSG
play move []   = calculatePosition [move]
                  >> return [move]
play move game = checkForTwoPasses game
                  >> calculatePosition (game ++ [move])
                  >> return (game ++ [move])


-- | Starts a new Go game with the first move.
go :: MoveSG -> Either String GameSG
go move = play move []

-- | Sugar for comfortable concatenation of several moves.
-- Example: go (b 16 16) // w 4 4 // b 16 4 // w 4 16
(//) :: Either String GameSG -> MoveSG -> Either String GameSG
(//) game move = game >>= play move


-- | Were there two passes in a row? Then the game is finished.
checkForTwoPasses :: GameSG -> Either String ()
checkForTwoPasses [] = Right ()
checkForTwoPasses [_] = Right ()
checkForTwoPasses (m1:m2:ms)
  | m1^.doing == Pass && m2^.doing == Pass = Left "Illegal move: The game is finished after two passes in a row."
  | otherwise = checkForTwoPasses (m2:ms)

-- | Calculates the position resulting from a list of moves, provided that all moves are legal.
calculatePosition :: [MoveSG] -> Either String PositionSG
calculatePosition moves = help moves (Right emptyBoard)
  where
    help :: [MoveSG] -> Either String PositionSG -> Either String PositionSG
    help []     position = position
    help (m:ms) position = help ms (position >>= (makeMove m))


-- | The core method that applies the Go rules to every move, returning either
-- the resulting position or the reason why a move is illegal.
makeMove :: MoveSG -> PositionSG -> Either String PositionSG
makeMove move position = do
    -- TODO OMS: end turn after two passes in a row
    checkPlayersTurn move position
    checkCoordinates move position
    checkPointIsFree move position
    checkObeysKoLock move position

    let newPosition = position  & placeStone move
                                & removeCapturedAndSetKoLock move

    checkIsNoSuicide move newPosition
    return $ endTurn move newPosition

-- | Is the right player playing?
checkPlayersTurn :: MoveSG -> PositionSG -> Either String ()
checkPlayersTurn move pos =
  if move^.color == pos^.nextToMove
  then Right ()
  else Left $ "Illegal move " ++ (show $ pos^.movesPlayed + 1) ++ ": " 
              ++ (show $ pos^.nextToMove) ++ " is to play next."


-- | Does the move contain legal coordinates?
checkCoordinates :: MoveSG -> PositionSG -> Either String ()
checkCoordinates (MoveSG _ Pass        ) pos = Right ()
checkCoordinates (MoveSG _ (Play (x,y))) pos =
  if x `elem` [1..19] && y `elem` [1..19]
  then Right ()
  else Left $ "Illegal move " ++ (show $ pos^.movesPlayed + 1) ++ ": "
              ++ "Coordinates must be in range [1..19]."


-- | Is the point played at free?
checkPointIsFree :: MoveSG -> PositionSG -> Either String ()
checkPointIsFree (MoveSG _ Pass        ) pos = Right ()
checkPointIsFree (MoveSG _ (Play point)) pos =
  if status pos point == Free
  then Right ()
  else Left $ "Illegal move " ++ (show $ pos^.movesPlayed + 1) ++ ": "
              ++ "There is already a stone at " ++ show point ++ "."


-- | Is the point locked because of the Ko rule?
checkObeysKoLock :: MoveSG -> PositionSG -> Either String ()
checkObeysKoLock (MoveSG _ Pass        ) pos = Right ()
checkObeysKoLock (MoveSG _ (Play point)) pos =
  if Just point /= pos^.lockedByKo
  then Right ()
  else Left $ "Illegal move " ++ (show $ pos^.movesPlayed + 1) ++ ": "
              ++ "The point " ++ show point ++ " is locked by Ko."


-- | Prevents that a stone is played such that it has no liberties
-- (after removing captured stones, if any).
checkIsNoSuicide :: MoveSG -> PositionSG -> Either String ()
checkIsNoSuicide (MoveSG _ Pass        ) pos = Right ()
checkIsNoSuicide (MoveSG _ (Play point)) pos =
  if ((> 0) <$> (liberties pos point)) == Right True
  then Right ()
  else Left $ "Illegal move " ++ (show $ pos^.movesPlayed + 1) ++ ": "
              ++ "This would be suicide."


-- | Adds the played stone to the position, if the player is not passing.
placeStone :: MoveSG -> PositionSG -> PositionSG
placeStone (MoveSG _     Pass        ) pos = pos
placeStone (MoveSG Black (Play point)) pos = pos & over blackStones ((:) point)
placeStone (MoveSG White (Play point)) pos = pos & over whiteStones ((:) point)


-- | Removes stones that are captured by the current move, if any (but does not regard
-- other stones possibly not removed erroneously), and sets the correct Ko lock.
removeCapturedAndSetKoLock :: MoveSG -> PositionSG -> PositionSG
removeCapturedAndSetKoLock      (MoveSG _     Pass        ) pos = pos & set lockedByKo Nothing
removeCapturedAndSetKoLock move@(MoveSG color (Play point)) pos =
  let stonesToCheck = filter (\pt -> status pos pt == (Taken $ other color)) (adiacentPoints point)
      capturedStones = nub . concat . (map (identifyGroup pos)) $
                          filter (\x -> (liberties pos x) == Right 0) stonesToCheck
      newKoLock = determineKoLock pos move capturedStones
  in  if color == Black
      then pos & over whiteStones (\stones -> stones \\ capturedStones)
               & over blackCaptures ( + (length capturedStones))
               & set lockedByKo newKoLock
      else pos & over blackStones (\stones -> stones \\ capturedStones)
               & over whiteCaptures ( + (length capturedStones))
               & set lockedByKo newKoLock


-- | In case of a Ko, calculates the lock set by the given move.
determineKoLock :: PositionSG -> MoveSG -> [Point] -> Maybe Point
determineKoLock pos (MoveSG _     Pass)         _                = Nothing
determineKoLock pos (MoveSG color (Play point)) capturedThisMove =
  if ((length capturedThisMove) == 1)
      && all (\pt -> (status pos pt) == Taken (other color)) (adiacentPoints point \\ capturedThisMove)
      && all (\pt -> (status pos pt) == Taken color)         (adiacentPoints (head capturedThisMove))
  then Just (head capturedThisMove)
  else Nothing


-- | Ends a turn in three steps: sets the correct 'pass' flag,
-- increments the number of moves played, and updates the color to move next.
endTurn :: MoveSG -> PositionSG -> PositionSG
endTurn move pos = pos & set lastPassed (move^.doing == Pass)
                       & over movesPlayed (+ 1)
                       & over nextToMove  (other)


-- | Counts the liberties that a stone at the given point has
-- (together with stones belonging to the same group).
liberties :: PositionSG -> Point -> Either String Int
liberties pos point = case (status pos point) of
  Free          -> Left "Counting liberties not possible: There is no stone at the given point."
  (Taken color) ->
    let group = identifyGroup pos point
    in  return $ length
          . filter ((==) Free)    -- count the number of free points ...
          . map (status pos)
          . nub                   -- ... (each only once) ...
          . concat
          . (map adiacentPoints)  -- ... adiacent to the group
          $ group


-- | Calculates the distance between two points along the grid (equivalent to Manhattan distance).
distance :: Point -> Point -> Int
distance p1 p2 = abs ((fst p1) - (fst p2)) + abs ((snd p1) - (snd p2))


-- | Returns the points adiacent to a given point, respecting edges and corners.
adiacentPoints :: Point -> [Point]
adiacentPoints point = [(x,y) | x <- [1..19], y <- [1..19], 
                                abs (x - fst point) + abs (y - snd point) == 1]


-- | Returns a list of all stones belonging to the group of a stone at the given point (if any).
identifyGroup :: PositionSG -> Point -> [Point]
identifyGroup pos point = case (status pos point) of
  Free -> []
  (Taken color) -> help (pos `stones` color) [point]
    where
      help :: [Point] -> [Point] -> [Point]
      help []      confirmed = confirmed
      help _       []        = []
      help toCheck confirmed =
        let newConfirmed = filter (\point -> (minimum ((distance point) <$> confirmed)) < 2) toCheck
        in  if newConfirmed == []
            then confirmed
            else help (toCheck \\ newConfirmed) (union confirmed newConfirmed)



-- Examples:

-- Shusaku (Black) versus Gennan Inseki (White), famous for the "ear reddening move"
earReddening = parseSGF earReddeningGame_sgf

-- Fourth game between Lee Sedol (White) and AlphaGo (Black)
sedolAlphaGo4 = parseSGF sedolAlphaGo_4_sgf
