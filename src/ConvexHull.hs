
module ConvexHull where

import Point

import qualified Data.Set as Set
import qualified Data.List as List

import Prelude hiding (Left, Right)


type Pt = (Double, Double)

c :: Colour
c = Red

pts :: [Point]
pts =
  Point 1 1 c :
  Point 1 2 c :
  Point 3 2 c :
  Point 6 6 c :
  Point 5 2 c :
  Point 6 7 c :
  Point 4 7 c :
  Point 3 9 c :
  Point 11 6 c :
  Point 5 6 c :
  Point 8 9 c : []

toFile :: IO ()
toFile =
  writeFile "pts.txt" $
    concatMap (\(Point a b _) -> show a ++ " " ++ show b ++ "\n") pts


convexHull :: [Point] -> [Point]
convexHull [] = []
convexHull pts@(p:_) =
  map (\(a, b) -> Point (round a) (round b) c) (graham dpts)
  where dpts = map (\(Point a b _) -> (fromIntegral a, fromIntegral b)) pts
        c = colour p

data Direct =
  Straight | Left | Right deriving (Show, Eq)


dir :: Pt -> Pt -> Pt -> Direct
dir (a0, a1) (b0, b1) (c0, c1)
  | d == 0 = Straight
  | d > 0  = Left
  | d < 0  = Right
  where d = (b0 - a0)*(c1 - a1) - (b1 - a1)*(c0 - a0)

polarAngle :: Pt -> Double
polarAngle (x, y)
  | x > 0 && y >= 0  = atan (y / x)
  | x > 0 && y < 0   = atan (y / x) + 2*pi
  | x < 0            = atan (y / x) + pi
  | x == 0 && y > 0  = pi / 2
  | x == 0 && y < 0  = 3*pi / 2
  | x == 0 && y == 0 = 0

lowest2left :: [Pt] -> Pt
lowest2left ps = go (10^9, 10^9) ps
  where go (x, y) [] = (x, y)
        go (x, y) ((px, py):ps)
          | py < y               = go (px, py) ps
          | py == y && px < x    = go (px, py) ps
          | otherwise            = go (x, y) ps

pminus, padd :: Pt -> Pt -> Pt
pminus (a, b) (c, d) = (a-c, b-d)
padd (a, b) (c, d)   = (a+c, b+d)

unique :: (Ord a) =>  [a] -> [a]
unique = Set.toList . Set.fromList

qsort f [] = []
qsort f (x:xs) =
  qsort f (filter (g (<=) x) xs) ++ [x] ++ qsort f (filter (g (>) x) xs)
  where g cmp x = (`cmp` (f x)) . f

remove _ [] = []
remove x (p:ps)
       | x == p = remove x ps
       | otherwise = p:(remove x ps)

remove_list _ [] = []
remove_list [] l = l
remove_list (x:xs) l = remove_list xs (remove x l)

graham :: [Pt] -> [Pt]
graham ps = l2l : (map (`padd` l2l) (go rest stack))
  where l2l = lowest2left ps
        pstmp = map (`pminus` l2l) (unique ps)
        grouped = map (qsort (\(x, y) -> x^2 + y^2)) (List.groupBy f pstmp)
        f x y = polarAngle x == polarAngle y
        clean = qsort polarAngle (map last grouped)

        go [] s = s
        go (x:xs) (top:ntt:s)
          | dir ntt top x /= Left = go (x:xs) (ntt:s)
          | otherwise             = go xs (x:top:ntt:s)

        stack = reverse (take 3 clean)
        rest = drop 3 clean
