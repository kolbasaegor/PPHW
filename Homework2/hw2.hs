import Data.List
import Data.Maybe
import CodeWorld

-- | A line with a focus.
-- Line xs y zs represents a descrete line:
-- * xs represents all elements to the left (below)
-- * y is the element in focus
-- * zs represents all elements after (above)
data Line a = Line [a] a [a]
  deriving (Show)

-- | A line of integers with focus at 0.
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- Exercise 1.1
-- | Keep up to a given number of elements in each direction in a line. 
-- cutLine 3 integers = Line [-1,-2,-3] 0 [1,2,3]
cutLine :: Int -> Line a -> Line a
cutLine 0 (Line _ focus _) = Line [] focus []
cutLine numElements (Line left focus right) =
  Line (take numElements left) focus (take numElements right)

-- Exercise 1.2
-- | Generate a list of elements of type a.
-- (generateUntil f x) creates new elements while
-- the value of (f x) is Just a and adds the element
-- to the end of the list.
-- * f is function that create new elemet
-- * x is initial value
generateUntil :: (a -> Maybe a) -> a -> [a]
generateUntil f x = helper [] f (f x)
  where
    helper :: [a] -> (a -> Maybe a) -> Maybe a -> [a]
    helper res f x =
      case x of
        Just x  -> helper (x:res) f (f x)
        Nothing -> reverse res

-- | Generate a line by using generating functions.
-- (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce
-- a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to
-- produce a list of elements to the right of x. 
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g =
  Line (generateUntil f x) x (generateUntil g x)

-- Exercise 1.3
-- | Apply a function to all elements on a line.
-- mapLine (^2) integers = Line [1, 4, 9, ..] 0 [1, 4, 9, ..]
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line left focus right) =
  Line (map f left) (f focus) (map f right)

-- Exercise 1.4
-- | Zip together two lines.
-- zipLines integers integers
--   = Line [(-1,-1),(-2,-2),..] (0,0) [(1,1),(2,2),..]
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line left1 f1 right1) (Line left2 f2 right2) =
  Line (zip left1 left2) (f1, f2) (zip right1 right2)

-- | Zip together two lines with a given combining function.
-- zipLinesWith (*) integers integers
--   = Line [1,4,9,..] 0 [1,4,9,..]
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line left1 f1 right1) (Line left2 f2 right2) =
  Line (zipWith f left1 left2) (f f1 f2) (zipWith f right1 right2)

-- Exercise 1.5
-- | Cell can be Dead or Alive
data Cell = Alive | Dead
  deriving (Show)

-- | Convert Cell to Int
c2i :: Cell -> Int
c2i Dead  = 0
c2i Alive = 1

-- | Computing the next state of the cell in
-- focus, according to Rule 30.
-- assume that [] == Dead
-- rule30 (Line [Dead] Alive [Alive]) = Alive
rule30 :: Line Cell -> Cell
rule30 (Line (Alive:left) Alive _)            = Dead
rule30 (Line (Dead:left) Alive _)             = Alive
rule30 (Line (Alive:left) Dead (Alive:right)) = Dead
rule30 (Line (Alive:left) Dead (Dead:rigt))   = Alive
rule30 (Line (Dead:left) Dead (Alive:right))  = Alive
rule30 (Line (Dead:left) Dead (Dead:right))   = Dead
rule30 (Line [] Dead (Dead:right))            = Dead
rule30 (Line [] Alive _)                      = Alive
rule30 (Line [] Dead (Alive:right))           = Alive
rule30 (Line (Dead:left) Dead [])             = Dead
rule30 (Line (Alive:left) Dead [])            = Alive

-- Exercise 1.6
-- | Shift focus on a line one position
-- to the left (if possible)
-- shiftLeft (Line [0, 1, 2] 3 [4, 5]) =
--   Just (Line [0, 1] 2 [3, 4, 5])
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] _ _) = Nothing
shiftLeft (Line (newFocus:left) focus right) =
  Just (Line left newFocus (focus:right))

-- | Shift focus on a line one position
-- to the right (if possible)
-- shiftRight (Line [0, 1, 2] 3 [4, 5]) =
--   Just (Line [0, 1, 2, 3] 4 [5])
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ _ []) = Nothing
shiftRight (Line left focus (newFocus:right)) =
  Just (Line (focus:left) newFocus right)

-- Exercise 1.7
-- | Maps every cell in a line into a version of 
-- the original line where that cell is in focus.
-- The new line of lines have the original line in focus.
lineShifts :: Line a -> Line (Line a)
lineShifts origLine = 
  genLine shiftLeft origLine shiftRight

-- | Apply rule30 to each shifted version of
-- the line to get the new state for each cell.
-- applyRule30 (Line [Dead] Alive [Dead]) =
--   Line [Alive] Alive [Alive]
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- Exercise 1.8
-- | Render a list of 1x1 pictures.
renderPictures :: [Picture] -> Picture
renderPictures [] = blank
renderPictures (pic:pics) =
  pic <> translated 1 0 (renderPictures pics)

-- | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
renderLine (Line left focus right) =
  renderPictures (reverse left) <>
  translated (fromIntegral (length left)) 0 focus <>
  translated ((fromIntegral (length left))+1) 0 (renderPictures right)

-- | Render a Cell
-- Dead  -> White square
-- Alive -> Black square
renderCell :: Cell -> Picture
renderCell Dead = rectangle 1 1
renderCell Alive = solidRectangle 1 1

-- | Render the fist N steps of Rule 30,
-- applied to a given starting line.
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 _ = blank
renderRule30 num line =
  renderLine (mapLine renderCell line) <>
  translated 0 (-1)
  (renderRule30 (num - 1) (applyRule30 line))

-- Exercise 1.10
-- | A descrete 2D space with a focus.
-- A 2D space is merely a (vertical) line
-- where each element is a (horizontal) line.
data Space a = Space (Line (Line a))
  deriving (Show)

-- | Apply a function to all elements on a space.
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space (Line left focus right)) =
  Space (Line
  (map (mapLine f) left)
  (mapLine f focus)
  (map (mapLine f) right)
  )

-- | Zip together two spaces.
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line left1 focus1 right1))
          (Space (Line left2 focus2 right2)) =
  Space (Line
    (zipWith zipLines left1 left2)
    (zipLines focus1 focus2)
    (zipWith zipLines right1 right2)
  )

-- | Zip together two spaces with a given combining function.
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith f (Space (Line left1 focus1 right1))
                (Space (Line left2 focus2 right2)) =
  Space (Line
    (zipWith (zipLinesWith f) left1 left2)
    (zipLinesWith f focus1 focus2)
    (zipWith (zipLinesWith f) right1 right2)
  )

-- Exercise 1.12
-- | Returns focus of the line.
-- focusOfLine (Line [] 1 []) = 1
focusOfLine :: Line a -> a
focusOfLine (Line _ focus _) = focus

-- | Returns focus of the space
focusOfSpace :: Space a -> a
focusOfSpace (Space (Line _ (Line _ f _) _)) = f

-- | Count how many alive neighbours
-- line have. ([] == Dead)
-- Focus itself is neighbour too.
-- liveNeighboursLine (Line [Alive] Alive [Dead]) = 2
liveNeighboursLine :: Line Cell -> Int
liveNeighboursLine (Line [] focus (rightCell:_)) =
  (c2i focus) + (c2i rightCell)
liveNeighboursLine (Line (leftCell:_) focus []) =
  (c2i leftCell) + (c2i focus)
liveNeighboursLine (Line [] focus []) =
  c2i focus
liveNeighboursLine (Line (leftCell:_) focus (rightCell:_)) =
  (c2i leftCell) + (c2i focus) + (c2i rightCell)

-- | Count how many alive neighbours
-- line have. ([] == Dead)
-- liveNeighboursLine (Line [Alive] Alive [Dead]) = 1
liveNeighboursFocusLine :: Line Cell -> Int
liveNeighboursFocusLine line =
  (liveNeighboursLine line) - (c2i (focusOfLine line))

-- | Counts number of live neighbours
-- for focus cell. ([] == Dead)
liveNeighbours :: Space Cell -> Int
liveNeighbours (Space (Line [] focus [])) =
  liveNeighboursFocusLine focus
liveNeighbours (Space (Line (leftLine:_) focus [])) =
  (liveNeighboursLine leftLine) +
  (liveNeighboursFocusLine focus)
liveNeighbours (Space (Line [] focus (rightLine:_))) =
  (liveNeighboursLine rightLine) +
  (liveNeighboursFocusLine focus)
liveNeighbours (Space (Line (leftLine:_) focus (rightLine:_))) =
  (liveNeighboursLine leftLine) +
  (liveNeighboursLine rightLine) +
  (liveNeighboursFocusLine focus)

-- | Computing the next state of the cell in focus,
-- according to the rules of Conway’s Game of Life.
conwayRule :: Space Cell -> Cell
conwayRule space =
  case (focusOfSpace space) of
    Alive  -> case (liveNeighbours space) of
      2 -> Alive
      3 -> Alive
      _ -> Dead
    Dead -> case (liveNeighbours space) of
      3 -> Alive
      _ -> Dead

-- Exercise 1.13
-- | Shift focus line on a space one position
-- to the left (if possible).
shiftLeftSpace :: Space a -> Maybe (Space a)
shiftLeftSpace (Space (Line [] _ _)) = Nothing
shiftLeftSpace (Space (Line (newFocus:left) focus right)) =
  Just (Space (Line left newFocus (focus:right)))

-- | Shift focus line on a space one position
-- to the right (if possible).
shiftRightSpace :: Space a -> Maybe (Space a)
shiftRightSpace (Space (Line _ _ [])) = Nothing
shiftRightSpace (Space (Line left focus (newFocus:right))) =
  Just (Space (Line (focus:left) newFocus right))

-- | Shift focus of the all lines on a space
-- one position to the left (if possible).
shiftFocusLeft :: Space a -> Maybe (Space a)
shiftFocusLeft (Space line2D) =
  case (shiftLeft (focusOfLine line2D)) of
    Just newLine -> Just(
      Space(mapLine fromJust (mapLine shiftLeft line2D)))
    Nothing      -> Nothing

-- | Shift focus of the all lines on a space
-- one position to the right (if possible).
shiftFocusRight :: Space a -> Maybe (Space a)
shiftFocusRight (Space line2D) =
  case (shiftRight (focusOfLine line2D)) of
    Just newLine -> Just(
      Space(mapLine fromJust (mapLine shiftRight line2D)))
    Nothing      -> Nothing

-- | Maps every cell in a focus line into a version of 
-- the original space where that cell is in focus.
-- The new line of spaces have the original space in focus.
focusShifts :: Space a -> Line (Space a)
focusShifts space =
  genLine shiftFocusLeft space shiftFocusRight

-- | Converts each cell in a discrete space into
-- a version of the original space with focus shifted
-- to that cell. The new space (of spaces)
-- must have the original space in focus.
spaceShifts :: Space a -> Space (Space a)
spaceShifts space =
  Space
    (mapLine focusShifts (genLine shiftLeftSpace space shiftRightSpace))

-- | Apply conwayRule to each shifted version
-- of the space to get new state for every cell.
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

-- Exercise 1.14
-- | Render a array of lines.
renderLines :: [Line Picture] -> Picture
renderLines [] = blank
renderLines (line:ls) =
  (renderLine line) <>
  translated 0 (-1) (renderLines ls)

-- | Render a space of 1x1 pictures.
-- center of picture is the focus of space
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line left focus right)) =
  blank <>
  translated
  (-(fromIntegral(length left)))
  (fromIntegral(length left))
  (renderLines (reverse left)) <>
  translated (-(fromIntegral(length left))) 0 (renderLine focus) <>
  translated (-(fromIntegral(length left))) (-1) (renderLines right)

type Time = Double

-- | Space along with the time elapsed
-- since the last zeroing of time.
data SpaceTime a = SpaceTime Time (Space a)
  deriving (Show)

-- | Render a space of cells.
renderSpaceTime :: SpaceTime Cell -> Picture
renderSpaceTime (SpaceTime _ space) =
  renderSpace (mapSpace renderCell space)

-- | Updates the cell space if one second has passed,
-- otherwise returns the unchanged space.
updateSpaceOverTime :: Event -> SpaceTime Cell -> SpaceTime Cell
updateSpaceOverTime (TimePassing dt) (SpaceTime time space)
  | (time + dt) >= 1 = SpaceTime 0 (applyConwayRule space)
  | otherwise        = SpaceTime (time + dt) space
updateSpaceOverTime _ spaceTime = spaceTime

-- | Animate Conway's Game of Life,
-- starting with a given space
-- and updating it every second.
animateConway :: Space Cell -> IO ()
animateConway initSpace = activityOf
  (SpaceTime 0 initSpace) updateSpaceOverTime renderSpaceTime

----------------------------------- Test ---------------------------------------
rule30init :: Line Cell
rule30init = Line
  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]
  Alive
  [Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead]

cs9x9 :: Space Cell
cs9x9 = Space (Line
   [
   Line [Dead, Dead, Dead, Dead] Alive [Dead, Dead, Dead, Dead],
   Line [Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead],
   Line [Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead],
   Line [Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead]
   ]
   (Line [Dead, Dead, Dead, Dead] Dead [Alive, Dead, Dead, Dead])
   [
   Line [Alive, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead],
   Line [Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead],
   Line [Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead],
   Line [Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead]
   ]
 )

main :: IO ()
main = animateConway cs9x9
-- main = drawingOf (renderRule30 10 rule30init)
