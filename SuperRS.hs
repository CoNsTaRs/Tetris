module SuperRS where

  import Tetromino
  import Playfield
  import RotationSystem
  import SuperRSData

  {-
     This module implements the Super Rotation System as a RotationSystem

       * Spawn:     spawnSRS
       * Rotate:    rotateSRS
       * Wall Kick: wallKickSRS

     See: https://tetris.wiki/SRS
  -}

  superRS :: RotationSystem
  superRS = RS spawnSRS rotateSRS wallKickSRS where

    -- Spawn a Mino
    spawnSRS :: Spawn
    spawnSRS shape = Mino (origStateSRS shape Spw) shape Spw

    -- Rotation rules defined by SRS
    rotateSRS :: Rotate
    rotateSRS _ mino@(Mino [] _ _)      = mino  -- Should never happen
    rotateSRS d (Mino ((x, y) : _) s r) = Mino area s rotation where

      rotatedOrig :: Area
      rotatedOrig = case d of
        CW -> origStateSRS s (rotationNext r)
        CC -> origStateSRS s (rotationPrev r)

      calcOffset :: Coord -> Coord -> Offset
      calcOffset (x', y') (p, q) = (x' - p, y' - q)

      applyOffset :: Coord -> Offset -> Coord
      applyOffset (x', y') (p, q) = (x' + p, y' + q)

      offset :: Coord
      offset = calcOffset (x, y) (head (origStateSRS s r))

      area :: Area
      area = fmap (applyOffset offset) rotatedOrig

      rotation :: Rotation
      rotation = case d of
        CW -> rotationNext r
        CC -> rotationPrev r

    -- Wall kick rules defined by SRS
    wallKickSRS :: WallKick
    wallKickSRS _  mino@(Mino _ ShpO _) _ = Just mino
    wallKickSRS pf mino@(Mino a s    r) d = offset >>= applyOffset mino where

      collision :: Playfield -> Area -> Offset -> Bool
      collision _   []            _      = False
      collision pf' ((x, y) : as) (p, q)
        = blockCollision || wallCollision || collision pf' as (p, q) where
          blockCollision   = (x + p, y + q) `elem` pf'
          xOverLowerBound  = x + p < 0
          xOverHigherBound = x + p > fieldWidth
          yOverHigherBound = y + q > fieldHeight
          wallCollision = xOverLowerBound || xOverHigherBound || yOverHigherBound

      safe :: Playfield -> Area -> Offset -> Bool
      safe x y z = not (collision x y z)

      possibilities :: [Offset]
      possibilities = filter (safe pf a) (wallKickDataSRS s r d)

      offset :: Maybe Offset
      offset = case possibilities of
        []      -> Nothing
        (x : _) -> Just x

      applyOffset :: Mino -> Offset -> Maybe Mino
      applyOffset (Mino a' s' r') y = Just (Mino (applyOffset' a' y) s' r') where

        applyOffset' :: Area -> Offset -> Area
        applyOffset' [] _ = []
        applyOffset' ((x', y') : as) (p, q) = (x' + p, y' + q) : applyOffset' as (p, q)
