module SuperRS where

  import Tetromino
  import Playfield
  import RotationSystem
  import SuperRSData

  superRS :: RotationSystem
  superRS = RS spawnStateSRS rotateSRS wallKickSRS where

    -- The spawn state defined by SuperRS
    spawnStateSRS :: SpawnState
    spawnStateSRS shape = origStateSRS shape Spw

    -- Rotation rules defined by SuperRS
    rotateSRS :: Rotate
    rotateSRS s r d ((x, y) : _) = fmap (applyOffset offset) rotatedOrig where

      rotatedOrig :: Area
      rotatedOrig = case d of
        CC -> origStateSRS s (rotationNext r)
        CW -> origStateSRS s (rotationPrev r)

      calcOffset :: Coord -> Coord -> Offset
      calcOffset (x, y) (p, q) = (x - y, p - q)

      applyOffset :: Coord -> Offset -> Coord
      applyOffset (x, y) (p, q) = (x + p, y + q)

      offset :: Coord
      offset = calcOffset (x, y) (head (origStateSRS s r))

    -- Wall kick rules defined by SuperRS
    wallKickSRS :: WallKick
    wallKickSRS _  ShpO _ _ a = Just a
    wallKickSRS pf s    r d a = offset >>= (applyOffset a) where
      
      collision :: Playfield -> Area -> Offset -> Bool
      collision [] _ _ = False
      collision pf ((p, q) : as) (r, s) = elem (p + r, q + s) pf || collision pf as (r, s)
      
      possibilities :: [Offset]
      possibilities = filter (collision pf a) (wallKickDataSRS s r d)
      
      offset :: Maybe Offset
      offset = case possibilities of
        []      -> Nothing
        (x : _) -> Just x

      applyOffset :: Area -> Offset -> Maybe Area
      applyOffset x y = Just (applyOffset' x y) where

        applyOffset' :: Area -> Offset -> Area
        applyOffset' [] _ = []
        applyOffset' ((x, y) : as) (p, q) = (x + p, y + q) : (applyOffset' as (p, q))
