{-# LANGUAGE TupleSections #-}
module Solitaire where

import Data.List
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Maybe

data Tile = Blank | Hole | Peg
  deriving (Eq, Ord, Show)

newtype Board = Board (Array (Int,Int) Tile)
  deriving (Eq, Ord, Show)

data Direction = North | East | South | West
  deriving (Enum, Show)

isPlayingLocation :: (Int, Int) -> Bool
isPlayingLocation (x,y) = x >= 0 && y >= 0 && x <= 6 && y <= 6 &&
                          ((x >= 2 && x <= 4) || (y >= 2 && y <= 4))

isBlankLocation :: (Int, Int) -> Bool
isBlankLocation = not . isPlayingLocation

startingTile :: (Int, Int) -> Tile
startingTile (3,3) = Hole
startingTile p
  | isBlankLocation p = Blank
  | otherwise = Peg

startingBoard :: Board
startingBoard = Board (array ((0,0), (6,6)) [ (p, startingTile p) | p <- allLocations ])

pieceAt :: (Int, Int) -> Board -> Maybe Tile
pieceAt position (Board board) = do
  guard (isPlayingLocation position)
  return (board ! position)

newPositions (x,y) North  = ((x,y+1), (x,y+2))
newPositions (x,y) East   = ((x+1,y), (x+2,y))
newPositions (x,y) South  = ((x,y-1), (x,y-2))
newPositions (x,y) West   = ((x-1,y), (x-2,y))

setPiece :: (Int, Int) -> Tile -> Board -> Board
setPiece l t (Board b) = Board (b // [(l,t)])

move :: (Int, Int) -> Direction -> Board -> Maybe Board
move position direction board = do
  Peg  <- pieceAt position board
  let (jumpLocation, endLocation) = newPositions position direction
  Peg  <- pieceAt jumpLocation board
  Hole <- pieceAt endLocation board

  return . setPiece position Hole
         . setPiece jumpLocation Hole
         . setPiece endLocation Peg
         $ board

allLocations :: [(Int,Int)]
allLocations = range ((0,0), (6,6))

locationsOnBoard :: [(Int,Int)]
locationsOnBoard = filter isPlayingLocation allLocations

nextBoards :: Board -> [((Int,Int), Direction, Board)]
nextBoards board
  = catMaybes [ (location, direction,) <$> move location direction board
              | location <- locationsOnBoard
              , direction <- [North ..]
              ]


mirrorCoordinate :: (Int, Int) -> (Int, Int)
mirrorCoordinate (x,y) = (x, 6-y)

rotateCoordinate :: (Int, Int) -> (Int, Int)
rotateCoordinate (x,y) = (y, -x+6)

coordinateTransforms :: [(Int, Int) -> (Int, Int)]
coordinateTransforms = base ++ (map (mirrorCoordinate .) base)
  where
    base = [id,
            rotateCoordinate,
            rotateCoordinate . rotateCoordinate,
            rotateCoordinate . rotateCoordinate . rotateCoordinate ]

applyCoordinateTransorm :: Board -> ((Int, Int) -> (Int, Int)) -> Board
applyCoordinateTransorm (Board board) transform
  = Board (array ((0,0), (6,6)) [ (p, board ! transform p) |  p <- allLocations ])


removeSymmetries :: [((Int, Int), Direction, Board)]
                 -> [((Int, Int), Direction, Board)]
removeSymmetries [] = []
removeSymmetries (this@(p,d,b):bs)
  = this : removeSymmetries [ them | them@(_,_,b') <- bs, b' `notElem` boardVariants ]
  where
    boardVariants = map (applyCoordinateTransorm b) coordinateTransforms


search :: Int -> Board -> [((Int,Int), Direction)]
              -> [ [((Int, Int), Direction)] ]
search 1 b chain
  | isFinal b = [ reverse chain ]
  | otherwise = []
search n b chain = [ chain'
                   | (p,d,b') <- nextBoards b
                   , chain' <- search (n-1) b' ((p,d):chain)
                   , (not . null) chain'
                   ]

isFinal :: Board -> Bool
isFinal board = pieceAt (3,3) board == Just Peg

printSolution :: IO ()
printSolution = mapM_ print (search 32 startingBoard [])

solution :: [((Int, Int), Direction)]
solution = [ ((1, 3), East), ((2, 1), North), ((0, 2), East), ((0, 4), South),
             ((2, 3), South), ((2, 0), North), ((2, 4), West), ((2, 6), South),
             ((3, 2), West), ((0, 2), East), ((3, 0), North), ((3, 2), West),
             ((3, 4), South), ((3, 6), South), ((3, 4), West), ((0, 4), East),
             ((4, 2), West), ((1, 2), East), ((4, 0), North), ((4, 3), South),
             ((5, 4), West), ((4, 6), South), ((6, 2), West), ((3, 2), East),
             ((6, 4), South), ((6, 2), West), ((4, 1), North), ((4, 3), North),
             ((2, 4), East), ((4, 5), South), ((5, 3), West)
           ]

solutionBoards :: [Board]
solutionBoards = scanl (\b (p, d) -> fromJust (move p d b))
                       startingBoard solution
