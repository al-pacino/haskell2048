import Control.Applicative
import System.Random
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowSize         = (600, 600)
windowPosition     = (100, 100)
windowCaption      = "2048"
windowPadding      = 10
gameOverText       = scale 0.4 0.5 $ Color (makeColor8 224 0 0 255)
                        (Text "Game Over!")
windowBackground   = makeColor8 128 92 64   255
boardPadding       = 10
boardColor1        = makeColor8 187 173 160 255
boardColor2        = makeColor8 87  73  60  200
cellColors         = [ makeColor8 204 192 179 255, makeColor8 238 228 218 255    
                     , makeColor8 238 228 218 255, makeColor8 237 224 200 255
                     , makeColor8 242 177 121 255, makeColor8 245 149 99  255
                     , makeColor8 246 124 95 255, makeColor8 237 207 114 255
                     , makeColor8 237 204 97 255, makeColor8 237 200 80  255
                     , makeColor8 237 197 63 255, makeColor8 237 194 46 255
                     ]

stp :: Int -> Int
stp n | n <= 1 = 0
      | otherwise = 1 + stp (n `div` 2)

getColor :: Int -> Color
getColor i = cellColors !! stp i

type Game = ([[Int]], Int, [Int])

calcCellSize :: (Int, Int) -> Int
calcCellSize (a, b) = (min a b - 2 * windowPadding - 5 * boardPadding) `div` 4

draw :: Game -> Picture
draw (g, s, r) =
  let
    fi = fromIntegral
    rs = 4 * s + 5 * boardPadding
    fr = \c (x, y) (w, h) ->
            translate x y (Color c (rectangleSolid (fi w) (fi h)))
    fp = \a k -> fi (s + boardPadding) * (fi a - k)
    fc =
      \v x y ->
        if v == 0 then (fr (head cellColors) (fp x 1.5, fp y 1.5) (s, s))
        else Pictures $
          [(fr (getColor v) (fp x 1.5, fp y 1.5) (s, s))
          , translate (fp x 1.9) (fp y 1.7) (scale 0.4 0.5 (Text (show v)))]
  in
    Pictures $ [(fr boardColor1 (0, 0) (rs, rs))] ++
      (foldr (++) []
        (zipWith (\v y -> foldr (:) []
            (zipWith (\v x -> fc v x y) v [0..3])) g [0..3])) ++
       (if null r then [Pictures $
            [(fr boardColor2 (0, 0) (rs, rs))
            , translate (fp 0 1) 0 gameOverText]] else [])

handle :: Event -> Game -> Game
handle (EventResize ns) (g, s, rs) = (g, calcCellSize ns, rs)
handle (EventKey (SpecialKey KeyUp)    Down _ _) g = go up    g
handle (EventKey (SpecialKey KeyDown)  Down _ _) g = go down  g
handle (EventKey (SpecialKey KeyLeft)  Down _ _) g = go left  g
handle (EventKey (SpecialKey KeyRight) Down _ _) g = go right g
handle _ g = g

go :: ([[Int]] -> [[Int]]) -> Game -> Game
go f (g, s, rs) =
  let
    g' = f g; a = up g; b = down g; c = left g; d = right g
  in
    if a == b && b == c && c == d then (g, s, [])
    else if g == g' then (g, s, rs)
         else let (g'', rs') = put rs g' in (g'', s, rs')

put :: [Int] -> [[Int]] -> ([[Int]], [Int])
put (a:b:rs) xss =
  let
    n = if a < 2 then 4 else 2
    x = b `mod` 4
    y = b `div` 4
    bss = take y xss
    (ms:ess) = drop y xss
    ms' = take x ms ++ [n] ++ drop (x+1) ms
  in
    if ms !! x == 0 then (bss ++ [ms'] ++ ess, rs) else put rs xss

up    = transpose   . right . transpose
down  = transpose   . left  . transpose
right = map reverse . left  . map reverse

left :: [[Int]] -> [[Int]]
left xss = map f xss
  where
    f xs = take 4 $ q (foldl g ([], 0) xs) ++ (repeat 0)
    q (xs, x) = xs ++ [x]
    g s@(ys, a) b
      | a > 0 && a == b = (ys ++ [a * 2], 0)
      | a > 0 && b > 0 = (ys ++ [a], b)
      | a > 0 = s
      | otherwise = (ys, b)

main =
  do
    rands <- genRandomList
    let
      display = InWindow windowCaption windowSize windowPosition
      (init, rands') = put rands $ replicate 4 [0, 0, 0, 0]
      begin = (init, calcCellSize windowSize, rands')
    play display windowBackground 0 begin draw handle $ flip const

genRandomList :: IO [Int]
genRandomList = randomRs (0, 15) <$> getStdGen
