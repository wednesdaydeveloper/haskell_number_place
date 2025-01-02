module Main where
import Data.Maybe (listToMaybe)
import MyLib (parseCell, getGridString, solve)
import Control.Monad (when)

main :: IO ()
main = do
  let initialGridStr = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 6, 0, 0, 0, 5, 9, 0],
        [4, 0, 0, 3, 0, 7, 0, 0, 2],
        [7, 0, 0, 0, 5, 0, 0, 0, 1],
        [8, 0, 0, 0, 0, 0, 0, 0, 9],
        [0, 6, 0, 0, 0, 0, 0, 7, 0],
        [0, 0, 5, 0, 0, 0, 6, 0, 0],
        [0, 0, 0, 2, 0, 3, 0, 0, 0],
        [0, 0, 0, 0, 7, 0, 0, 0, 0]
        ]
  let initialGrid = map (map parseCell) initialGridStr
  putStrLn "初期盤面:"
  putStrLn $ getGridString initialGrid
  let solutions = solve initialGrid
  case listToMaybe solutions of
    Nothing -> putStrLn "解が見つかりませんでした"
    Just solution -> do
      putStrLn "唯一の解:"
      putStrLn $ getGridString solution
  when (length solutions > 1) $ putStrLn "複数解あります"