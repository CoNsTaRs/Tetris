module RotationSystem where

  import Tetromino
  import Playfield
  
  type Offset = (Int, Int)
  
  wallKickData :: Shape -> Rotation -> Dir -> [Offset]
  wallKickData s r d = case (s, (r, d)) of
    (ShpO, (_,   _))  -> [(0, 0)]
    (ShpI, (Spw, CW)) -> [(0, 0), (-2, 0), ( 1, 0), (-2, -1), ( 1,  2)]
    (ShpI, (Rht, CC)) -> [(0, 0), ( 2, 0), (-1, 0), ( 2,  1), (-1, -2)]
    (ShpI, (Rht, CW)) -> [(0, 0), (-1, 0), ( 2, 0), (-1,  2), ( 2, -1)]
    (ShpI, (Rev, CC)) -> [(0, 0), ( 1, 0), (-2, 0), ( 1, -2), (-2,  1)]
    (ShpI, (Rev, CW)) -> [(0, 0), ( 2, 0), (-1, 0), ( 2,  1), (-1, -2)]
    (ShpI, (Lft, CC)) -> [(0, 0), (-2, 0), ( 1, 0), (-2, -1), ( 1,  2)]
    (ShpI, (Lft, CW)) -> [(0, 0), ( 1, 0), (-2, 0), ( 1, -2), (-2,  1)]
    (ShpI, (Spw, CC)) -> [(0, 0), (-1, 0), ( 2, 0), (-1,  2), ( 2, -1)]
    (_, (Spw, CW)) -> [(0, 0), (-1, 0), (-1,  1), (0, -2), (-1, -2)]
    (_, (Rht, CC)) -> [(0, 0), ( 1, 0), ( 1, -1), (0,  2), ( 1,  2)]
    (_, (Rht, CW)) -> [(0, 0), ( 1, 0), ( 1, -1), (0,  2), ( 1,  2)]
    (_, (Rev, CC)) -> [(0, 0), (-1, 0), (-1,  1), (0, -2), (-1, -2)]
    (_, (Rev, CW)) -> [(0, 0), ( 1, 0), ( 1,  1), (0, -2), ( 1, -2)]
    (_, (Lft, CC)) -> [(0, 0), (-1, 0), (-1, -1), (0,  2), (-1,  2)]
    (_, (Lft, CW)) -> [(0, 0), (-1, 0), (-1, -1), (0,  2), (-1,  2)]
    (_, (Spw, CC)) -> [(0, 0), ( 1, 0), ( 1,  1), (0, -2), ( 1, -2)]
  
  wallKick :: Playfield -> Shape -> Rotation -> Dir -> Area -> Maybe Area
  wallKick _  ShpO _ _ a = Just a
  wallKick pf s    r d a = offset >>= (applyOffset a) where

    maybeHead :: [a] -> Maybe a
    maybeHead []      = Nothing
    maybeHead (x : _) = Just x
    
    collision :: Playfield -> Area -> Offset -> Bool
    collision [] _ _ = False
    collision pf ((p, q) : as) (r, s) = elem (p + r, q + s) pf || collision pf as (r, s)
    
    possibilities :: [Offset]
    possibilities = filter (collision pf a) (wallKickData s r d)
    
    offset :: Maybe Offset
    offset = maybeHead possibilities

    applyOffset :: Area -> Offset -> Maybe Area
    applyOffset x y = Just (applyOffset' x y) where

      applyOffset' :: Area -> Offset -> Area
      applyOffset' [] _ = []
      applyOffset' ((x, y) : as) (p, q) = (x + p, y + q) : (applyOffset' as (p, q))
