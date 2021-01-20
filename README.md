# Multiplication Circle
This Haskell program animates the multiplication circle for different numbers on a circle with  200 points.

# Gloss
This module requires the Gloss library for graphics.Installation:
>	cabal update
>	cabal install gloss

# Start
Program can be run by navigating into this directory and opening ghci:
>	Prelude> :l multiplication_circles.hs
>	Prelude> main

This will run the animation with default values.

# Parameters
There are 3 parameters influencing the animation. They are located in the multiplication_circles.hs file. Their default values are

Radius of the circle 
> radius = 400

Number of points on the circle
> n = 200

Rate of change - seconds per change of the number used for multiplication by 1
> rate = 2
