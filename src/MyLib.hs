module MyLib (parseCell, printGrid, solve) where

import Data.Maybe (isNothing)

type Grid = [[Maybe Int]]

-- グリッドを表示する関数
printGrid :: Grid -> IO ()
printGrid grid = do
  mapM_ (putStrLn . concatMap (\cell -> maybe "." show cell ++ " ")) grid
  putStrLn ""

-- 空いているマスを探す関数
findEmpty :: Grid -> Maybe (Int, Int)
findEmpty = findEmpty' 0 0
  where
    findEmpty' :: Int -> Int -> Grid -> Maybe (Int, Int)
    findEmpty' row col g
      | row == 9 = Nothing
      | col == 9 = findEmpty' (row + 1) 0 g
      | isNothing (g !! row !! col) = Just (row, col)
      | otherwise = findEmpty' row (col + 1) g

-- 数字を置けるか確認する関数
isValid :: Grid -> (Int, Int) -> Int -> Bool
isValid grid (row, col) num =
  not (checkRow || checkCol || checkBlock)
  where
    checkRow :: Bool
    checkRow = any (\x -> x == Just num) (grid !! row)
    checkCol :: Bool
    checkCol = any (\r -> (grid !! r !! col) == Just num) [0 .. 8]
    blockRow :: Int
    blockRow = row `quot` 3 -- divをquotに変更
    blockCol :: Int
    blockCol = col `quot` 3 -- divをquotに変更
    checkBlock :: Bool
    checkBlock = any (\r -> any (\c -> (grid !! (blockRow * 3 + r) !! (blockCol * 3 + c)) == Just num) [0 .. 2]) [0 .. 2]

-- バックトラッキングを行う関数
solve :: Grid -> [Grid]
solve grid =
  case findEmpty grid of
    Nothing -> [grid] -- 全てのマスが埋まった
    Just (row, col) -> do
      num <- [1 .. 9]
      if isValid grid (row, col) num
        then
          let newGrid = replace grid (row, col) (Just num)
          in solve newGrid
        else []

-- グリッドの要素を置き換えるヘルパー関数
replace :: Grid -> (Int, Int) -> Maybe Int -> Grid
replace grid (row, col) val = take row grid ++ [take col (grid !! row) ++ [val] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

-- 文字列をMaybe Intに変換する関数
parseCell :: Char -> Maybe Int
parseCell '0' = Nothing
parseCell c | '1' <= c && c <= '9' = Just (read [c])
parseCell _ = error "Invalid input"
