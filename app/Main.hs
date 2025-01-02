module Main where
import Data.Maybe (listToMaybe)
import MyLib (parseCell, printGrid, solve)
import Control.Monad (when)

main :: IO ()
main = do
  let initialGridStr = [
        "000000000",
        "016000590",
        "400307002",
        "700050001",
        "800000009",
        "060000070",
        "005000600",
        "000203000",
        "000070000"
        ]
  let initialGrid = map (map parseCell) initialGridStr
  putStrLn "初期盤面:"
  printGrid initialGrid
  let solutions = solve initialGrid
  case listToMaybe solutions of
    Nothing -> putStrLn "解が見つかりませんでした"
    Just solution -> do
      putStrLn "唯一の解:"
      printGrid solution
  when (length solutions > 1) $ putStrLn "複数解あります"