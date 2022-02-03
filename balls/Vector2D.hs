module Vector2D where

data V2 = V2
    { x :: Float
    , y :: Float
    }
    deriving Show

add :: V2 -> V2 -> V2
add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

diff :: V2 -> V2 -> V2
diff (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

mult :: V2 -> Float -> V2
mult (V2 x y) m = V2 (x * m) (y * m)

dot :: V2 -> V2 -> Float
dot (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2

magSq :: V2 -> Float
magSq (V2 x y) = x ^ 2 + y ^ 2

mag :: V2 -> Float
mag = sqrt . magSq

normalize :: V2 -> V2
normalize v2 = v2 `mult` (1 / mag v2)

setMag :: V2 -> Float -> V2
setMag v2 newMag = normalize v2 `mult` newMag
