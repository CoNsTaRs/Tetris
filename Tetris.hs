module Tetris where

  import Data.List
  import System.Random
  import Control.Monad
  import Tetromino
  import Playfield
  import RotationSystem


  -- Left shift for one step
  moveLft :: Playfield -> Mino -> Mino
  moveLft pf (Mino ar s r) = Mino area s r where

    tryLft :: Area -> Bool
    tryLft [] = False  -- Should never happen
    tryLft ((x, y) : as)
      | x == 0 = False
      | null as = (x - 1, y) `notElem` pf
      | otherwise = ((x - 1, y) `notElem` pf) && tryLft as

    shiftLft :: Coord -> Coord
    shiftLft (x, y) = (x - 1, y)

    area :: Area
    area = if tryLft ar then map shiftLft ar else ar


  -- Right shift for one step
  moveRht :: Playfield -> Mino -> Mino
  moveRht pf (Mino ar s r) = Mino area s r where

    tryRht :: Area -> Bool
    tryRht [] = False  -- Should never happen
    tryRht ((x, y) : as)
      | x == fieldWidth = False
      | null as = (x + 1, y) `notElem` pf
      | otherwise = (x + 1, y) `notElem` pf && tryRht as

    shiftRht :: Coord -> Coord
    shiftRht (x, y) = (x + 1, y)

    area :: Area
    area = if tryRht ar then map shiftRht ar else ar


  -- Soft drop
  softDrop :: Playfield -> Mino -> Mino
  softDrop pf (Mino ar s r) = Mino ar' s r where

    tryDwn :: Area -> Bool
    tryDwn [] = False  -- Should never happen
    tryDwn ((x, y) : as)
      | y == fieldHeight = False
      | null as  = (x, y + 1) `notElem` pf
      | otherwise = (x, y + 1) `notElem` pf && tryDwn as

    shiftDwn :: Coord -> Coord
    shiftDwn (x, y) = (x, y + 1)

    ar' :: Area
    ar' = if tryDwn ar then map shiftDwn ar else ar


  -- Hard drop
  hardDrop :: Playfield -> Mino -> Playfield
  hardDrop pf (Mino ar _ _) = pf ++ fmap (drop' height) ar where

    drop' :: Int -> Coord -> Coord
    drop' n (x, y) = (x, y + n)

    height :: Int
    height = foldl min 19 (map cellHeight ar) where

      cellHeight :: Coord -> Int
      cellHeight (_, y) = foldl min 19 (map snd (filter ((y ==) . fst) pf))


  -- Rotate clockwise or counter-clockwise
  rotate :: RotationSystem -> Playfield -> Mino -> Dir -> Mino
  rotate _ _ mino@(Mino _ ShpO _) _ = mino
  rotate (RS _ rsRotate rsWallKick) pf mino dir = applyWallKick attempt where

    rotated :: Mino
    rotated = rsRotate dir mino

    attempt :: Maybe Mino
    attempt = rsWallKick pf rotated dir

    applyWallKick :: Maybe Mino -> Mino
    applyWallKick Nothing  = mino
    applyWallKick (Just x) = x


  -- Find lines to clear after a Mino lands
  findFullLines :: Playfield -> [Int]
  findFullLines pf = findFullLines' pf [] where

    findFullLines' :: Playfield -> [Int] -> [Int]
    findFullLines' [] _ = []
    findFullLines' pf'@((_, y) : ps) ls
      | null pf' = ls
      | null ls  = [y | isFullLine (fieldWidth - 1) y ps]
      | null ps && y `elem` ls = ls
      | null ps && isFullLine (fieldWidth - 1) y ps = y : ls
      | null ps = ls
      | y `elem` ls = findFullLines' ps ls
      | isFullLine (fieldWidth - 1) y ps = findFullLines' ps (y : ls)
      | otherwise = findFullLines' ps ls
      where
        isFullLine :: Int -> Int -> Playfield -> Bool
        isFullLine _ _ []            = False
        isFullLine 0 _ _             = True
        isFullLine _ _ [_]           = False
        isFullLine n l ((_, y') : xs)
          | y' == l = isFullLine (n - 1) l xs
          | otherwise = isFullLine n l xs


  -- To clear lines from the line numbers given
  clearLines :: Playfield -> [Int] -> Playfield
  clearLines xs []       = xs
  clearLines xs (y : ys) = clearLines (clearLine xs y) ys where

    clearLine :: Playfield -> Int -> Playfield
    clearLine []           _ = []
    clearLine ls@[(_, y')] n = if n == y' then [] else ls
    clearLine    ((x, y') : ys') n
      | n == y'   = clearLine ys' n
      | otherwise = (x, y') : clearLine ys' n


  -- Naive line clear gravity
  lineClearGravityNaive :: Playfield -> [Int] -> Playfield
  lineClearGravityNaive pf xs = removeLines pf (sort xs) where

    removeLines :: Playfield -> [Int] -> Playfield
    removeLines = foldl removeLine where

      removeLine :: Playfield -> Int -> Playfield
      removeLine pf' l = map dropHigherLines pf' where

        dropHigherLines :: Coord -> Coord
        dropHigherLines (x, y) = (x, if y < l then y - 1 else y)

  -- Sticky line clear gravity
  -- lineClearGravitySticky :: Playfield -> Playfield
  -- lineClearGravitySticky =


  -- Random Generation using bag system
  randomGenerate :: IO [Shape]
  randomGenerate = liftM2 (!!) (return bags) randomIndex where

    randomIndex :: IO Int
    randomIndex = randomRIO (0, 5039)

    bags :: [[Shape]]
    bags = permutations [ShpI, ShpO, ShpS, ShpZ, ShpL, ShpJ, ShpT]
