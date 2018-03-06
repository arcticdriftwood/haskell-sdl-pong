{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Foldable
import Data.Time.Clock.System
import Data.Word
import Foreign.C.Types
import Lens.Micro
import Lens.Micro.Platform
import qualified SDL
import SDL.Time
import SDL.Vect

targetDT :: Float
targetDT = 1.0 / 120.0

type Position = V2 Float

type Velocity = V2 Float

type Size = V2 Float

type Score = (Int, Int)

type CollisionBox = (Float, Float, Float, Float)

type DeltaTime = Float

data Paddle = Paddle
    { _pPosition :: Position
    , _pVelocity :: Velocity
    , _pSize :: Size
    , _pSpeed :: Float
    } deriving (Show)

data Ball = Ball
    { _bPosition :: Position
    , _bVelocity :: Velocity
    , _bSize :: Size
    , _bSpeed :: Float
    } deriving (Show)

data PState = PState
    { _paddles :: (Paddle, Paddle)
    , _ball :: Ball
    , _score :: Score
    , _isPlaying :: Bool
    } deriving (Show)

makeLenses ''PState

makeLenses ''Ball

makeLenses ''Paddle

white = V4 maxBound maxBound maxBound maxBound

black = V4 0 0 0 maxBound

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

getX (V2 x _) = x

getY (V2 _ y) = y

if' x y z =
    if x
        then y
        else z

getNS :: IO Float
getNS =
    liftA2
        (/)
        (fromIntegral . systemNanoseconds <$> getSystemTime)
        (pure 1e9 :: IO Float)

simpleVelocity :: Position -> Velocity -> DeltaTime -> Position
simpleVelocity (V2 x y) (V2 x' y') dt = V2 (x + (x' * dt)) (y + (y' * dt))

bound :: (Float -> Float -> Bool) -> Float -> V2 Float -> V2 Float
bound c l (V2 x y) = V2 x (if' (y `c` l) l y)

lockToScreen :: Size -> (V2 Float -> V2 Float)
lockToScreen size =
    bound (<) 0 . bound (>) (fromIntegral screenHeight - getY size)

velocityOverPosition :: DeltaTime -> Velocity -> Position -> Position
velocityOverPosition dt' v' p' = simpleVelocity p' v' dt'

doesCollide :: CollisionBox -> CollisionBox -> Bool
doesCollide (x, y, w, h) (x', y', w', h') =
    x < x' + w' && x + w > x' && y < y' + h' && y + h > y'

yDiff :: Paddle -> Ball -> Float
yDiff p b = (yB - yP) / (getY (p ^. pSize) / 2)
  where
    yP = getY (p ^. pPosition) + (getY (p ^. pSize) / 2)
    yB = getY (b ^. bPosition) + (getY (b ^. bSize) / 2)

class Collidable a where
    toCollisionBox :: a -> CollisionBox
    collidesWith :: a -> CollisionBox -> Bool

instance Collidable Ball where
    toCollisionBox b = (x, y, w, h)
      where
        (V2 w h) = b ^. bSize
        (V2 x y) = b ^. bPosition
    collidesWith p = doesCollide (toCollisionBox p)

instance Collidable Paddle where
    toCollisionBox p = (x, y, w, h)
      where
        (V2 w h) = p ^. pSize
        (V2 x y) = p ^. pPosition
    collidesWith p = doesCollide (toCollisionBox p)

class Movable a where
    applyMovement :: Float -> a -> a

instance Movable Ball where
    applyMovement dt b = over bPosition f $ b
      where
        f = velocityOverPosition dt (b ^. bVelocity)

instance Movable Paddle where
    applyMovement dt p = over pPosition f p
      where
        f = lockToScreen (p ^. pSize) . velocityOverPosition dt (p ^. pVelocity)

createNewState =
    PState
        ( Paddle (V2 620 100) (V2 0 0) (V2 10 100) 400
        , Paddle (V2 10 100) (V2 0 0) (V2 10 100) 400)
        (Ball (V2 320 320) (V2 100 0) (V2 10 10) 200)
        (0, 0)
        False

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    window <-
        SDL.createWindow
            "SDL"
            SDL.defaultWindow
            {SDL.windowInitialSize = V2 screenWidth screenHeight}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.showWindow window
    frame window renderer createNewState =<< getNS
    SDL.destroyWindow window
    SDL.quit

frame :: SDL.Window -> SDL.Renderer -> PState -> Float -> IO ()
frame window renderer state pt = do
    ct <- getNS
    let dt =
            if ct - pt > 0
                then ct - pt
                else 1 + ct - pt
    events <- map SDL.eventPayload <$> SDL.pollEvents
    keyMap <- SDL.getKeyboardState
    let newstate =
            if'
                (view isPlaying state)
                (scorePoints . runCollision $ over ball (applyMovement dt) .
                 over paddles (applyMovement dt *** applyMovement dt) .
                 over
                     paddles
                     (\(p, p') -> psplit $ runInput keyMap directions [p, p']) $
                 state)
                (set isPlaying True .
                 set
                     (ball . bPosition)
                     (V2
                          (fromIntegral screenWidth / 2)
                          (fromIntegral screenHeight / 2)) $
                 state)
    when
        (fst (state ^. score) /= fst (newstate ^. score) || snd (state ^. score) /=
         snd (newstate ^. score))
        (print $ newstate ^. score)
    let quit = SDL.QuitEvent `elem` events
    SDL.rendererDrawColor renderer SDL.$= white
    SDL.clear renderer
    drawObjects newstate renderer
    SDL.present renderer
    unless quit (frame window renderer newstate ct)
  where
    up k = k SDL.ScancodeUp
    down k = k SDL.ScancodeDown
    w k = k SDL.ScancodeW
    s k = k SDL.ScancodeS
    psplit x = (head x, last x)
    scancodes :: (SDL.Scancode -> Bool) -> [(Bool, Bool)]
    scancodes k = [(up k, down k), (w k, s k)]
    directions = [(V2 0 (-1), V2 0 1), (V2 0 (-1), V2 0 1)]
    applyInputToVelocity ::
           (Bool, Bool) -> (V2 Float, V2 Float) -> Paddle -> Paddle
    applyInputToVelocity b d p = do
        let s = p ^. pSpeed
        if' (fst b) (over pVelocity (df s $ fst d)) id .
            if' (snd b) (over pVelocity (df s $ snd d)) id .
            set pVelocity (V2 0 0) $
            p
      where
        df s (V2 x' y') (V2 x y) = V2 (x + x' * s) (y + y' * s)
    runInput ::
           (SDL.Scancode -> Bool) -- translates to [(Bool, Bool)]
        -> [(V2 Float, V2 Float)]
        -> [Paddle]
        -> [Paddle]
    runInput k d p = zipWith3 applyInputToVelocity (scancodes k) d p
    runCollision :: PState -> PState
    runCollision s = do
        let c = map (\p -> (collidesWith b . toCollisionBox $ p, p)) [p1, p2]
        if'
            (by < 0 || by > (fromIntegral screenHeight - getY (b ^. bSize)))
            (over (ball . bVelocity) (\(V2 x y) -> V2 x (-y)))
            id $
            foldl f s c
      where
        (p1, p2) = s ^. paddles
        b = s ^. ball
        by = getY $ b ^. bPosition
        speed = b ^. bSpeed
        f :: PState -> (Bool, Paddle) -> PState
        f s c =
            if'
                (fst c)
                (set (ball . bVelocity) (V2 (dir * speed) (dy * speed)) s)
                s
          where
            dir = if' (getX (b ^. bPosition) > getX (p ^. pPosition)) 1 (-1)
            col = fst c
            p = snd c
            dy = yDiff (snd c) b
    scorePoints :: PState -> PState
    scorePoints s =
        let bx = getX $ s ^. ball . bPosition
            bs = s ^. ball . bSpeed
            sw :: Float
            sw = fromIntegral screenWidth
        in if'
               (bx > sw)
               (set isPlaying False . over (score . _1) (+ 1) .
                set (ball . bVelocity) (V2 bs 0))
               (id) .
           if'
               (bx < 0)
               (set isPlaying False . over (score . _2) (+ 1) .
                set (ball . bVelocity) (V2 (-bs) 0))
               (id) $
           s
    drawObjects state renderer = do
        let (p1, p2) = (state ^. paddles)
        let b = state ^. ball
        SDL.rendererDrawColor renderer SDL.$= black
        mapM_
            (\(p, s) -> SDL.fillRect renderer (Just $ pToR p s))
            [ (p1 ^. pPosition, p1 ^. pSize)
            , (p2 ^. pPosition, p2 ^. pSize)
            , (b ^. bPosition, b ^. bSize)
            ]
      where
        pToR (V2 px py) (V2 pw ph) =
            SDL.Rectangle
                (P $ V2 (floor px) (floor py))
                (V2 (floor pw) (floor ph))
