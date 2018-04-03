module Main where

  import UI.NCurses
  import Tetromino
  import Tetris
  import RotationSystem
  import SuperRS

  f :: (Int, Int) -> Update ()
  f (x, y)
    | y >= 0 = moveCursor (toInteger y) (toInteger (2 * x)) *> drawString "██"
    | otherwise = moveCursor 0 0

  printMino :: Mino -> Update ()
  printMino (Mino (x : []) _ _) = f x
  printMino (Mino (x : xs) s r) = f x *> printMino (Mino xs s r)

  printField :: [(Int, Int)] -> Update ()
  printField (x : []) = f x
  printField (x : xs) = f x *> printField xs

  waitFor :: Window -> (Event -> Bool) -> Curses ()
  waitFor w p = loop where
    loop = do
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop
        Just ev' -> if p ev' then return () else loop

  main :: IO ()
  main = runCurses $ do
    setEcho False
    _ <- setCursorMode CursorInvisible
    w <- defaultWindow
    updateWindow w $ do
      printMino (rotate superRS [] (Mino [(0, -1), (0,  0), (0,  1), (0, 2)] ShpI Rht) CC)
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
