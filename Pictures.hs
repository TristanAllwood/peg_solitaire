{-# LANGUAGE NoMonomorphismRestriction #-}
module Pictures where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Maybe

import Solitaire


renderSolution = vcat' (with { sep = 0.1 }) (zipWith renderBoard solutionBoards (solution ++ [((0,0),South)]))

renderBoard board (p, d)
  = hcat [ vcat [ renderTile d p jump hole position tile #
                    (if position == p then
                      named "start"
                     else if position == hole then
                       named "end"
                     else id)
                | y <- [0..6]
                , let position = (x,y)
                , let tile = fromMaybe Blank (pieceAt position board)
                ]
         | x <- [0..6]
         ] # attach "start" "end"
  where
    (jump, hole) = newPositions p d


attach n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop (((location b1 ~~ location b2) # lc grey # lw 0.1) `atop` (circle 0.2 # fc grey # moveTo (location b2)))

renderTile _ _ _ _ _ Blank = square 2 # fc black # lw 0
renderTile d p jump hole position Hole  = (circle 0.75 # fc holeColour) `atop` links position `atop` tileBack
  where
    holeColour = black

renderTile d p jump hole position Peg   = (circle 0.75 # fc tileColour) `atop` links position `atop` tileBack
  where
    tileColour
      | p == position    = grey
      | jump == position = white
      | otherwise        = white

tileBack = square 2 # fc greenyellow # lw 0

links (x,y) = ((rotateBy (1/4) (line north ||| line south)) # centerY) `atop`
              ((line west  ||| line east) # centerX)
  where
    line p = hrule 1.0 # (if p then lc black # lw 0.1 else lw 0.1 # lcA transparent)

    north = isPlayingLocation (x, y+1)
    east  = isPlayingLocation (x+1, y)
    south = isPlayingLocation (x, y-1)
    west  = isPlayingLocation (x-1, y)
