import Data.Array
import Data.Array.IO
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

puzzleW = 8
puzzleH = 8
regions = newRegionMatrix (puzzleW, puzzleH)
   ["a","a","b","b","c","d","e","e"
   ,"a","a","f","b","g","d","d","e"
   ,"f","f","f","h","g","i","j","j"
   ,"k","k","k","h","g","i","i","j"
   ,"l","h","h","h","h","i","i","j"
   ,"l","m","n","n","n","o","p","j"
   ,"m","m","m","m","q","o","o","o"
   ,"r","q","q","q","q","o","s","s"]

regionCoords = Map.fromListWith (++)
   [(getRegion (x, y), [(x, y)]) | y <- [1..puzzleH], x <- [1..puzzleW]]

--------------------------------------------------------------------------------

main = do
   puzzle <- newPuzzleMatrix (puzzleW, puzzleH)
      [0, 0, 0, 0, 0, 0, 0, 0
      ,0, 1, 3, 0, 0, 0, 0, 0
      ,0, 0, 0, 0, 0, 3, 0, 0
      ,0, 0, 3, 0, 0, 0, 0, 0
      ,0, 5, 0, 3, 0, 0, 0, 0
      ,0, 2, 0, 0, 0, 0, 0, 0
      ,0, 0, 0, 0, 0, 0, 3, 0
      ,0, 0, 5, 3, 0, 0, 0, 0]

   solvable <- solve puzzle (1, 1)

   if solvable then
      printPuzzle puzzle
   else
      print "No solution found"

--------------------------------------------------------------------------------

solve puzzle (x, y)
   | y > puzzleH = return True
   | x > puzzleW = solve puzzle (1, y + 1)
   | otherwise = do
      value <- getPuzzle puzzle (x, y)

      if value == 0 then
         getAvailableNumbers puzzle (x, y) >>= tryNumbers puzzle (x, y)
      else
         solve puzzle (x + 1, y)

tryNumbers puzzle (x, y) [] = return False
tryNumbers puzzle (x, y) (guess:rest) = do
   setPuzzle puzzle (x, y) guess

   solution <- solve puzzle (x + 1, y)

   if solution then
      return True
   else
      setPuzzle puzzle (x, y) 0 >> tryNumbers puzzle (x, y) rest

--------------------------------------------------------------------------------

getAvailableNumbers puzzle (x, y) = do
   a <- getOrthogonalNumbers puzzle (x, y)
   b <- getRegionNumbers puzzle (x, y)
   c <- getVerticalNumbers puzzle (x, y)

   return (c \\ (a `union` b))

getOrthogonalNumbers puzzle (x, y) =
   mapM (getPuzzle puzzle) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getRegionNumbers puzzle (x, y) =
   mapM (getPuzzle puzzle) (getRegionCoords (x, y))

getVerticalNumbers puzzle (x, y) = do
   a <- tryGetRegionNeighbor puzzle (x, y) (x, y + 1) 0
   b <- tryGetRegionNeighbor puzzle (x, y) (x, y - 1) (getRegionSize (x, y) + 1)

   return [(a + 1)..(b - 1)]

--------------------------------------------------------------------------------

newRegionMatrix (w, h) = listArray (1, w * h)

getRegion coord = regions ! coordToIndex coord

getRegionCoords coord = regionCoords Map.! getRegion coord

getRegionSize coord = length (getRegionCoords coord)

inSameRegion a b = inbounds a && inbounds b && getRegion a == getRegion b

newPuzzleMatrix (w, h) = newListArray (1, w * h)

getPuzzle :: Num e => IOArray Int e -> (Int, Int) -> IO e
getPuzzle puzzle coord | inbounds coord = readArray puzzle (coordToIndex coord)
                       | otherwise = return 0

setPuzzle puzzle coord = writeArray puzzle (coordToIndex coord)

tryGetRegionNeighbor puzzle a b fallback | inSameRegion a b = getPuzzle puzzle b
                                         | otherwise = return fallback

printPuzzle puzzle =
   mapM_ (\y ->
      mapM_ (\x -> getPuzzle puzzle (x, y) >>= putStr . (++ " ") . show)
      [1..puzzleW] >> putStrLn "")
   [1..puzzleH]

inbounds (x, y) = x >= 1 && x <= puzzleW && y >= 1 && y <= puzzleH

coordToIndex (x, y) = (y - 1) * puzzleW + x