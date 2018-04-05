module Tetris where

  import Data.List
  import System.Random
  import Control.Monad

  import Tetromino
  import Playfield
  import RotationSystem

  {-
     This module includes the basic moves of the game, which are:

       * Move Left:       moveLft
       * Move Right:      moveRht
       * Soft Drop:       softDrop
       * Hard Drop:       hardDrop
       * Rotate:          rotate
       * Line Clear:      findFullLines + clearLines + lineClearGravityNaive
       * Random Generate: randomGenerate

     ** The sticky line-clear gravity is not yet implemented

     ** 'Hold' will not be presented because of the nature of the language **
     ** Deal with it in main instead **
  -}


  -- Left shift for one step
  moveLft :: Playfield -> Mino -> Mino
  moveLft pf (Mino ar s r) = Mino area s r where

    tryLft :: Area -> Bool
    tryLft [] = False  -- Should never happen
    tryLft ((x, y) : as)
      | x == 0 = False
      | null as = (x - 1, y) `notElem` coord pf
      | otherwise = ((x - 1, y) `notElem` coord pf) && tryLft as

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
      | x >= (fieldWidth - 1) = False
      | null as = (x + 1, y) `notElem` coord pf
      | otherwise = (x + 1, y) `notElem` coord pf && tryRht as

    shiftRht :: Coord -> Coord
    shiftRht (x, y) = (x + 1, y)

    area :: Area
    area = if tryRht ar then map shiftRht ar else ar


  -- Soft drop
  softDrop :: Playfield -> Mino -> Either Mino Playfield
  softDrop pf mino@(Mino ar s r)
    | tryDwn ar = Left (Mino (map mvDwn ar) s r)
    | otherwise = Right (appendMino pf mino)
      where
        tryDwn :: Area -> Bool
        tryDwn [] = False  -- Should never happen
        tryDwn ((x, y) : as)
          | y >= fieldHeight = False
          | null as  = (x, y + 1) `notElem` coord pf
          | otherwise = (x, y + 1) `notElem` coord pf && tryDwn as

        mvDwn :: Coord -> Coord
        mvDwn (x, y) = if y < fieldHeight then (x, y + 1) else (x, y)


  -- Hard drop
  hardDrop :: Playfield -> Mino -> (Playfield, Mino)
  hardDrop [] (Mino ar s r) = pack' (map dropToButtom ar) where

    pack' :: Area -> (Playfield, Mino)
    pack' ar' = (fmap (\(x, y) -> (x, y, s)) ar', Mino ar' s r)

    dropToButtom :: Coord -> Coord
    dropToButtom (x, y) = (x, y + offsetY) where
      offsetY :: Int
      offsetY = fieldHeight - maximum (map snd ar)

  hardDrop pf (Mino ar s r) = pack' (map (drop' height) ar) where

    pack' :: Area -> (Playfield, Mino)
    pack' ar' = (pf ++ fmap (\(x, y) -> (x, y, s)) ar', Mino ar' s r)

    drop' :: Int -> Coord -> Coord
    drop' n (x, y) = (x, y + n)

    height :: Int
    height = foldl min fieldHeight (map cellHeight ar) where

      cellHeight :: Coord -> Int
      cellHeight (x, y) = case ysBelow of
        [] -> fieldHeight - maximum (map snd ar)
        xs -> minimum xs
        where
          ysBelow =map ((\y' -> (y' - y) - 1) . snd)
            (filter ((y <) . snd) (filter ((x ==) . fst) (coord pf)))


  -- Rotate clockwise or counter-clockwise
  rotate :: RotationSystem -> Playfield -> Mino -> Dir -> Mino
  rotate _ _ mino@(Mino _ ShpO _) _ = mino
  rotate rs pf mino dir = applyWallKick attempt where

    rotated :: Mino
    rotated = rsRotate rs dir mino

    attempt :: Maybe Mino
    attempt = rsWallKick rs pf rotated dir

    applyWallKick :: Maybe Mino -> Mino
    applyWallKick Nothing  = mino
    applyWallKick (Just x) = x


  -- Find lines to clear after a Mino lands
  findFullLines :: Playfield -> [Int]
  findFullLines pf = findFullLines' pf [0..fieldHeight] where

    findFullLines' :: Playfield -> [Int] -> [Int]
    findFullLines' [] _ = []
    findFullLines' _ [] = []
    findFullLines' pf' (l : ls)
      | length (filter ((== l) . snd) (coord pf')) >= fieldWidth = l : findFullLines' pf' ls
      | otherwise = findFullLines' pf' ls


  -- To clear lines from the line numbers given
  clearLines :: Playfield -> [Int] -> Playfield
  clearLines pf [] = pf
  clearLines pf (l : ls) = clearLines (filter ((/= l) . snd3) pf) ls
    where
      snd3 :: (a, b, c) -> b
      snd3 (_, x, _) = x


  -- Naive line clear gravity
  lineClearGravityNaive :: Playfield -> [Int] -> Playfield
  lineClearGravityNaive [] _ = []
  lineClearGravityNaive pf [] = pf
  lineClearGravityNaive pf (l : ls) = lineClearGravityNaive (moved ++ orign) ls
    where
      snd3 :: (a, b, c) -> b
      snd3 (_, x, _) = x
      mvDwn :: Cell -> Cell
      mvDwn (x, y, z) = (x, y + 1, z)
      moved :: [Cell]
      moved = map mvDwn $ filter ((< l) . snd3) pf
      orign :: [Cell]
      orign = filter ((> l) . snd3) pf

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
