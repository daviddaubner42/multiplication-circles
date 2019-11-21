module Main where

-- ----------
-- Imports
-- ----------

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random
import Data.Fixed

-- -------------
-- Parameters
-- -------------

rate = 2        -- number of seconds per change of the multiplier by 1
n = 200         -- number of points on the circle
radius = 300    -- radius of the circle

-- -------------
-- Window Setup
-- -------------

width, height, offset :: Int
width = 800
height = 800
offset = 100

window :: Display
window = InWindow "Multiplication Circle" (width, height) (offset, offset)

background :: Color
background = black

-- ---------------
-- State
-- ---------------

data State = State {
  times :: Float,                           -- the number with which each of the 200 points is multiplied
  clr :: (Float, Float, Float, Float)       -- current color of lines connecting points with their products
  } deriving Show

initialState = State {
  times = 2,
  clr = (255, 77, 77, 255)
  }

-- ---------------
-- Dynamics
-- ---------------

-- List of colours of lines to transiton through with time
clrs = [ (255, 77, 77, 255), (255, 153, 102, 255), (230, 230, 0, 255), (102, 255, 51, 255),
         (77, 148, 255, 255), (51, 204, 52, 255), (255, 204, 0, 255), (255, 117, 26, 255) ]


-- Colour of the lines at any given moment as a function of time - transitions between 2 consecutive colours from clrs
transition :: State -> Float -> State
transition state seconds = state { clr = (dr, dg, db, a) }
  where
    (r, g, b, a) = clr state
    (r'', g'', b'', a'') = clrs !! (goal seconds)
    (r', g', b', a') = clrs !! (((goal seconds)-1) `mod` length clrs)
    dr = r' + (r''-r') * time
    dg = g' + (g''-g') * time
    db = b' + (b''-b') * time
    time = (mod' seconds rate) / rate

-- Index of the next colour to transition to as a function of time
goal :: Float -> Int
goal seconds = (((ceiling (seconds/rate))) `mod` length clrs)

-- Multiplier as a function of time with given rate of change
change :: Float -> State -> State
change seconds state = state {
  times = seconds/rate
  }

-- --------------
-- Render
-- --------------

-- Coorcinates of a point on a circle with a given angle
pointCoords :: Float -> (Float, Float)
pointCoords x = (radius * cos (pi - pi * ((360 / fromIntegral n) * x)/180), radius * sin (pi - pi * ((360 / fromIntegral n) * x)/180))

-- The render function
render :: State -> Picture
render state = pictures [
  translate 0 (-100) $ pictures [
    circle,
    connections
    ],
  timer
  ] where

      circle =
        color white $
        circleSolid radius

      points = pictures (map mkPoint [0..n-1])
      -- Helper function to draw a point given its coordinates
      mkPoint :: Int -> Picture
      mkPoint x =
        uncurry translate (pointCoords (fromIntegral x)) $
        color red $
        circleSolid 3

      connections = pictures (map makeLine [0..n-1])
      -- Helper function to draw a line connecting point with its multiple
      makeLine :: Int -> Picture
      makeLine x =
        color (makeColorI r' g' b' a') $
        line [ pointCoords (fromIntegral x), pointCoords (fromIntegral x * times state)]

      -- Colour of lines and timer
      (r, g, b, a) = clr state
      r' = round r
      g' = round g
      b' = round b
      a' = round a

      -- Shows current value of times
      timer =
        translate (-105) (5/6*radius) $
        scale 0.7 0.7 $
        color (makeColorI 130 42 150 255) $
        color (makeColorI r' g' b' a') $
        text (show (fromIntegral (round (100*(times state)))/100))

-- -----------
-- Main
-- -----------

main :: IO ()
main = animate window background frame
  where
    frame :: Float -> Picture
    frame seconds = render $ (transition (change seconds initialState) seconds)
