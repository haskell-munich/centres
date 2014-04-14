

module Perceptron where

import Point

import qualified System.Random as Random

import qualified Data.List as List

newtype Perceptron = Perceptron { unPerceptron :: [Double] } deriving (Show)

data Input = Input { clas :: Colour, unInput :: [Double] } deriving (Show)



p :: Perceptron
p = Perceptron [0, 0.1, 0.1]

i :: Input
i = Input Red [1, 2]


fix :: (Ord b, Fractional b) => (a -> b) -> [a] -> a
fix _ [] = error "no fix point"
fix f (x:y:ys) = if abs (f x - f y) < eps then y else fix f (y:ys)
  where eps = 10^^(-6)

learnUntilFix :: Perceptron -> [Input] -> Perceptron
learnUntilFix p input =
  fix (flip calculateError input) (take 100 $ iterateLearn p input)

perceptron :: [Point] -> (Point, Point)
perceptron pts = perceptronToLine $ learnUntilFix p (map pointToInput pts)

pointToInput :: Point -> Input
pointToInput (Point x y cl) = augmentInput (Input cl [fromIntegral x, fromIntegral y])


perceptronToLine :: Perceptron -> (Point, Point)
perceptronToLine (Perceptron [w0, w1, w2]) =
  (Point 0 (round (line u v 0)) Red, Point 800 (round (line u v 800)) Red)
  where u = (0, -w0/w2)
        v = (-w0/w1, 0)
        f (a, b) = Point a b Red

augmentInput :: Input -> Input
augmentInput (Input c is) = Input c (1:is)

evaluate :: Perceptron -> Input -> Colour
evaluate (Perceptron ps) (Input _ is) =
  if sum (zipWith (*) ps is) > 0 then Red else Green

adjust :: Double -> Perceptron -> Input -> Perceptron
adjust eta (Perceptron ps) (Input _ is) =
  Perceptron (zipWith (+) ps (map (*eta) is))

learn :: Perceptron -> Input -> Perceptron
learn p i@(Input cl _) =
  if evaluate p i == cl then p else adjust eta p i
  where eta = if cl == Red then 0.7 else (-0.7)

type Range = (Double, Double)

generateInput :: Int -> Int -> Colour -> Range -> Range -> [Input]
generateInput seed n cl xr yr =
  let (xgen, ygen) = Random.split $ Random.mkStdGen seed
      xs = Random.randomRs xr xgen
      ys = Random.randomRs yr ygen
  in take n $ zipWith (\x y -> Input cl [x, y]) xs ys



toFile :: [Input] -> IO ()
toFile = writeFile "pts.txt" . List.intercalate "\n" . map f
  where f (Input _ [_, x, y]) = show x ++ " " ++ show y

testInput :: [Input]
testInput = map augmentInput $ inp0 ++ inp1
  where inp0 = generateInput 0 len Red (0, 7) (4, 7)
        inp1 = generateInput 1 len Green (0, 7) (0, 3)
        len = 100


calculateError :: Perceptron -> [Input] -> Double
calculateError (Perceptron ps) is = (sum $ map (eucl ps . unInput) is) / len
  where eucl as = sum . map (^2) . zipWith (-) as
        len = fromIntegral (length is)



inpg = augmentInput $ Input Green [1, 1]
inpr = augmentInput $ Input Red [2, 2]

testSmall :: [Input]
testSmall = map augmentInput $ 
  Input Red [1, 1] :
  Input Red [2.1, 0.8] :
  Input Red [2.9, -3] :
  Input Green [4.5, 6.8] :
  Input Green [5.3, 4.8] :
  Input Green [6.2, 4.1] : []


iterateLearn :: Perceptron -> [Input] -> [Perceptron]
iterateLearn p input =
  iterate f p
  where f p = List.foldl' learn p input


{-

fix f x = if abs (y - f y) < eps then y else fix f y
  where y = f x
        eps = 10^(-6)
-}

line :: (Double, Double) -> (Double, Double) -> Double -> Double
line (x1, y1) (x2, y2) x = (x-x2)/(x1-x2)*y1 + (x-x1)/(x2-x1)*y2


plotLine :: Perceptron -> IO ()
plotLine (Perceptron [w0, w1, w2]) =
  writeFile "line.txt" (toString (0, line a b 0) ++ "\n" ++ toString (7, line a b 7) ++ "\n")
  where a = (0, -w0/w2)
        b = (-w0/w1, 0)
        toString (x, y) = show x ++ " " ++ show y
