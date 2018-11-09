module ClassicBoard where

import Grid

classic_board = Board 2 9 classic_groups

classic_groups :: [Group]
classic_groups = row_groups ++ col_groups ++ square_groups
  where
    row_groups = map (\y -> map (\x -> Position [x, y]) [1..9]) [1..9]
    col_groups = map (\x -> map (\y -> Position [x, y]) [1..9]) [1..9]
    square_groups = map (\g -> square_group g) [1..9]
    square_group n = map (\c -> Position [square_x n c, square_y n c]) [1..9]
    square_col n = if (mod n 3 > 0) then mod n 3 else 3
    square_row n = div (n + 2) 3
    square_x n c
      | mod c 3 > 0 = (square_col n - 1) * 3 + (mod c 3)
      | otherwise   = square_col n * 3
    square_y n c = (square_row n - 1) * 3 + div (c + 2) 3
