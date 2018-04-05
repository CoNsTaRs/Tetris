module Color where

  import Tetromino

  colorOf :: Shape -> String
  colorOf mino = case mino of
    ShpI -> "\ESC[1;36m"
    ShpO -> "\ESC[1;33m"
    ShpS -> "\ESC[0;32m"
    ShpZ -> "\ESC[1;31m"
    ShpL -> "\ESC[0;33m"
    ShpJ -> "\ESC[0;34m"
    ShpT -> "\ESC[0;35m"
