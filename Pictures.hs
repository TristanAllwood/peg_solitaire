{-# LANGUAGE NoMonomorphismRestriction #-}
module Pictures where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Maybe
import Diagrams.TwoD.Size (width)

import Solitaire

renderSolution = vcat' (with { sep = 0.1 }) renderedSolutionBoards

renderedSolutionBoards = zipWith renderBoard solutionBoards (solution ++ [((0,0),South)])


staircase
  = vcat' spacing
         [ left b1
         , five b2 b3 b4 b5 b6
         , right b7
         , five b12 b11 b10 b9 b8
         , left b13
         , five b14 b15 b16 b17 b18
         , right b19
         , five b24 b23 b22 b21 b20
         , left b25
         , five b26 b27 b28 b29 b30
         , right b31
         , right b32
         ]
  where

    spacing = with { sep = 1.0 }

    left d  = hcat' spacing (d : replicate 4 (strutX (width d)))      # centerX
    right d = hcat' spacing ((replicate 4 (strutX (width d))) ++ [d]) # centerX

    five a b c d e = hcat' spacing [a,b,c,d,e] # centerX

    [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,b31,b32 ] = renderedSolutionBoards


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
         ] # (if p /= (0,0) then attach "start" "end" else id)
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
