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
         [ left b0
         , left b1
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

    b0 = renderBoard startingBoard ((0,0), South)
    [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,b31,b32 ] = renderedSolutionBoards

spiral
  = mconcat [ b # translate (r2 (x,y))
            | (i,b) <- [0..] `zip` boardPics
            , let a = (width b * 2)
            , let theta = 2 * pi * (i / 16)
            , let r = a * sqrt theta
            , let x = r * cos theta
            , let y = r * sin theta
            ]
  where
    boardPics = b0 : renderedSolutionBoards
    b0 = renderBoard startingBoard ((0,0), South)


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
         ] # (if p /= (0,0) then attach "start" "end" else id) # centerX # centerY
        `atop` square 14 # fc greenyellow # lc green
  where
    (jump, hole) = newPositions p d


attach n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop (((location b1 ~~ location b2) # lc red # lw 0.2)
        `atop` (circle 0.2 # fc red # moveTo (location b1)
        `atop` (circle 0.15 # fc red # moveTo (location b2))))
renderTile _ _ _ _ _ Blank = tileBack
renderTile d p jump hole position Hole  = (circle 0.75 # fc holeColour) `atop` links position `atop` tileBack
  where
    holeColour = black

renderTile d p jump hole position Peg   = (circle 0.75 # fc tileColour) `atop` links position `atop` tileBack
  where
    tileColour
      | p == position    = grey
      | jump == position = grey
      | otherwise        = white

tileBack = strutX 2 `atop` strutY 2

links (x,y) = ((rotateBy (1/4) (line north ||| line south)) # centerY) `atop`
              ((line west  ||| line east) # centerX)
  where
    line p = hrule 1.0 # (if p then (lc black # lw 0.1) else (lw 0.1 # lcA transparent))

    north = isPlayingLocation (x, y+1)
    east  = isPlayingLocation (x+1, y)
    south = isPlayingLocation (x, y-1)
    west  = isPlayingLocation (x-1, y)
