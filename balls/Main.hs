module Main where

import           Graphics.Gloss                 ( Color
                                                , Display(InWindow)
                                                , Picture
                                                , circleSolid
                                                , color
                                                , makeColorI
                                                , pictures
                                                , play
                                                , translate
                                                )
import           Graphics.Gloss.Data.Color      ( Color
                                                , makeColorI
                                                )
import           Graphics.Gloss.Interface.IO.Game
                                                ( Color
                                                , Display(InWindow)
                                                , Event(EventKey, EventResize)
                                                , Key(MouseButton)
                                                , KeyState(Down)
                                                , MouseButton(LeftButton)
                                                , Picture
                                                , circleSolid
                                                , color
                                                , makeColorI
                                                , pictures
                                                , translate
                                                )
import           System.Random                  ( StdGen
                                                , newStdGen
                                                , uniformR
                                                )

import qualified Vector2D                      as V2
import           Vector2D                       ( V2(..) )

window :: Display
window = InWindow "Balls" (500, 500) (300, 150)

bg :: Color
bg = makeColorI 59 59 59 255

data Ball = Ball
    { ballPos    :: V2
    , ballVel    :: V2
    , ballRadius :: Float
    , ballColor  :: Color
    }
    deriving Show

data World = World
    { worldBalls :: [Ball]
    , worldSeed  :: StdGen
    , canvasSize :: (Float, Float)
    }

renderWorld :: World -> Picture
renderWorld world = pictures $ map ballToPicture balls
  where
    balls = worldBalls world
    ballToPicture ball =
        let (V2 x y) = ballPos ball
            r        = ballRadius ball
            c        = ballColor ball
        in  translate x y $ color c $ circleSolid r

eventHandler :: Event -> World -> World
eventHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) world =
    let balls           = worldBalls world
        seed0           = worldSeed world
        (c, seed1)      = randColor seed0
        (r, seed2)      = randRadius seed1
        (vx, vy, seed3) = randV seed2
        ball            = Ball (V2 x y) (V2 vx vy) r c
    in  world { worldBalls = ball : balls, worldSeed = seed3 }
  where
    randColor seed0 =
        let (red  , seed1) = uniformR (0, 256) seed0
            (green, seed2) = uniformR (0, 256) seed1
            (blue , seed3) = uniformR (0, 256) seed2
            color          = makeColorI red green blue 255
        in  (color, seed3)
    randRadius = uniformR (10, 30)
    randV seed0 =
        let (vx, seed1) = uniformR (-200, 200) seed0
            (vy, seed2) = uniformR (-200, 200) seed1
        in  (vx, vy, seed2)

eventHandler (EventResize size) world = world
    { canvasSize = pairFromIntegral size
    }
    where pairFromIntegral (a, b) = (fromIntegral a, fromIntegral b)

eventHandler _ w = w


updateWorld :: Float -> World -> World
updateWorld dt world =
    let balls'   = map updateBall balls
        balls''  = bounceBalls balls'
        balls''' = map applyGravity balls''
    in  world { worldBalls = map respectScreenBox balls''' }
  where
    balls = worldBalls world
    seed  = worldSeed world
    size  = canvasSize world

    updateBall (Ball pos vel@(V2 vx vy) r c) =
        let (V2 x' y') = pos `V2.add` (vel `V2.mult` dt)
            (w, h)     = canvasSize world
            vx'        = if x' < (-w / 2 + r) || x' > w / 2 - r then -vx else vx
            vy'        = if y' < (-h / 2 + r) || y' > h / 2 - r then -vy else vy
        in  Ball (V2 x' y') (V2 vx' vy') r c

    bounceBalls [] = []
    bounceBalls (ball : balls) =
        let (ball', balls') = bounceBall ball balls
        in  ball' : bounceBalls balls'

    bounceBall ball [] = (ball, [])
    bounceBall ball1 (ball2 : balls) =
        let (ball1', ball2') = bounceBallPair ball1 ball2
        in  ball2' `prependToSnd` bounceBall ball1' balls

    prependToSnd b (a, bs) = (a, b : bs)

    bounceBallPair b1 b2 = if not (b1 `intersects` b2)
        then (b1, b2)
        else
            let x1         = ballPos b1
                x2         = ballPos b2
                v1         = ballVel b1
                v2         = ballVel b2
                m1         = ballRadius b1 ^ 2
                m2         = ballRadius b2 ^ 2
                v1'        = afterCollision m1 m2 x1 x2 v1 v2
                v2'        = afterCollision m2 m1 x2 x1 v2 v1
                (b1', b2') = fixBallsPos b1 b2
            in  (b1' { ballVel = v1' }, b2' { ballVel = v2' })

    intersects b1 b2 =
        let x1 = ballPos b1
            x2 = ballPos b2
        in  V2.magSq (x1 `V2.diff` x2) < (ballRadius b1 + ballRadius b2) ^ 2

    afterCollision m1 m2 x1 x2 v1 v2 =
        let q   = 2 * m2 / (m1 + m2)
            n   = (v1 `V2.diff` v2) `V2.dot` (x1 `V2.diff` x2)
            d   = V2.magSq (x1 `V2.diff` x2)
            v1' = v1 `V2.diff` ((x1 `V2.diff` x2) `V2.mult` (q * n / d))
        in  v1'

    fixBallsPos b1 b2 =
        let pos1       = ballPos b1
            pos2       = ballPos b2
            dist       = V2.mag (pos1 `V2.diff` pos2)
            neededDist = ballRadius b1 + ballRadius b2
            nudge      = (neededDist - dist) / 2
            nudgeV     = (pos1 `V2.diff` pos2) `V2.setMag` nudge
            pos1'      = pos1 `V2.add` nudgeV
            pos2'      = pos2 `V2.diff` nudgeV
        in  if nudge < 0
                then (b1, b2)
                else (b1 { ballPos = pos1' }, b2 { ballPos = pos2' })

    respectScreenBox ball =
        let (V2 x y) = ballPos ball
            r        = ballRadius ball
            (w, h)   = size
            minX     = -w / 2 + r
            maxX     = w / 2 - r
            minY     = -h / 2 + r
            maxY     = h / 2 - r
            x' | x < minX  = minX
               | x > maxX  = maxX
               | otherwise = x
            y' | y < minY  = minY
               | y > maxY  = maxY
               | otherwise = y
        in  ball { ballPos = V2 x' y' }

    applyGravity ball =
        let vel  = ballVel ball
            vel' = vel `V2.add` V2 0 (-10)
        in  ball { ballVel = vel' }


main :: IO ()
main = do
    seed <- newStdGen
    play window
         bg
         60
         (World [] seed (500, 500))
         renderWorld
         eventHandler
         updateWorld
