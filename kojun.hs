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
regionCoords =
   let
      mapRegions (x, y) dict
         | y > puzzleH = dict
         | x > puzzleW = mapRegions (1, y + 1) dict
         | otherwise =
            mapRegions (x + 1, y) (Map.insertWith (++) (getRegion (x, y))
                                                       [(x, y)] dict)
   in do
      mapRegions (1, 1) Map.empty

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
   | otherwise =
      let    
         tryNumbers puzzle (x, y) [] = return False
         tryNumbers puzzle (x, y) (guess:rest) = do  
            setPuzzle puzzle (x, y) guess
 
            solution <- solve puzzle (x + 1, y)

            if solution then
               return True
            else
               setPuzzle puzzle (x, y) 0 >> tryNumbers puzzle (x, y) rest
      in do
         value <- getPuzzle puzzle (x, y)

         case value of
            0 -> getAvailableNumbers puzzle (x, y) >>= tryNumbers puzzle (x, y)
            _ -> solve puzzle (x + 1, y)

--------------------------------------------------------------------------------

getAvailableNumbers p (x, y) = do
   a <- getOrthogonalNumbers p (x, y)
   b <- getRegionNumbers p (x, y)
   c <- getVerticalNumbers p (x, y)

   return (c \\ (a `union` b))

getOrthogonalNumbers p (x, y) =
   mapM (getPuzzle p) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getRegionNumbers p (x, y) = mapM (getPuzzle p) (getRegionCoords (x, y))

getVerticalNumbers p (x, y) =
   let
      getRegionSize coord = length (getRegionCoords coord)
      inSameRegion a b = inbounds a && inbounds b && getRegion a == getRegion b
      tryGetRegionNeighbor a b fallback | inSameRegion a b = getPuzzle p b
                                        | otherwise = return fallback
   in do
      a <- tryGetRegionNeighbor (x, y) (x, y + 1) 0
      b <- tryGetRegionNeighbor (x, y) (x, y - 1) (getRegionSize (x, y) + 1)

      return [(a + 1)..(b - 1)]

--------------------------------------------------------------------------------

newRegionMatrix (w, h) = listArray (1, w * h)

getRegion coord = regions ! coordToIndex coord

getRegionCoords coord = regionCoords Map.! getRegion coord

newPuzzleMatrix (w, h) = newListArray (1, w * h)

getPuzzle :: Num e => IOArray Int e -> (Int, Int) -> IO e
getPuzzle p coord | inbounds coord = readArray p (coordToIndex coord)
                  | otherwise = return 0

setPuzzle p coord = writeArray p (coordToIndex coord)

printPuzzle p =
   let
      spot x = (putStr . show) x >> putStr " "
      line y = mapM_ (\x -> getPuzzle p (x, y) >>= spot) [1..puzzleW]
   in
      mapM_ (\y -> line y >> putStr "\n") [1..puzzleH]

inbounds (x, y) = x >= 1 && x <= puzzleW && y >= 1 && y <= puzzleH

coordToIndex (x, y) = (y - 1) * puzzleW + x