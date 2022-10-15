module ProblemSet7 where

import Text.Read (readMaybe)
import Data.Char (toUpper)

-- Exercise 1
-- This function manipulates the IO and returns
-- some value a (IO a) &
-- p takes a and returns bool value (a -> Bool) &
-- r takes a string and returns value
-- of type a and performs some IO action (String -> IO a)
-- => (a -> Bool) -> (String -> IO a) -> IO a
guess :: (a -> Bool) -> (String -> IO a) -> IO a
guess p g = do
  s <- getLine
  x <- g s
  case p x of
    True -> return x
    False -> guess p g

p' :: (Ord a, Num a) => a -> Bool
p' x
  | x < 10    = True
  | otherwise = False

prompt :: String -> IO Int
prompt msg = do
  print msg
  getInt

getInt :: IO Int
getInt = do
  line <- getLine
  case readMaybe line of
    Just x -> return x
    Nothing -> do
      putStrLn "Not a number"
      getInt

-- this example shows that guess type is defined correctly
example' = guess p' prompt

-- Exercise 2
toUpperString :: String -> String
toUpperString [] = []
toUpperString (c:cs) = (toUpper c) : (toUpperString cs)

echo :: IO ()
echo = do
  input <- getLine
  print (toUpperString input)
  echo

-- Exercise 3
-- 3.a
foreverIO :: IO a -> IO b
foreverIO program = do
  program
  foreverIO program

-- 3.b
whenIO :: Bool -> IO () -> IO ()
whenIO True program = program
whenIO False program = return ()

-- 3.c
maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO (Just program) = do
  val <- program
  return (Just val)
maybeIO Nothing = return Nothing

-- 3.d
sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO [] = return []
sequenceMaybeIO (x:xs) = do
  val <- x
  case val of
    Just val -> do
      other <- sequenceMaybeIO xs
      return (other ++ [val])
    Nothing  -> sequenceMaybeIO xs

-- 3.e
whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO func val = do
  val <- func val
  case val of
    Just val -> whileJustIO func val
    Nothing  -> return ()

-- 3.f
forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s
forStateIO_ s [] func = return s 
forStateIO_ s (x:xs) func = do
  s <- func x s
  forStateIO_ s xs func

verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
  putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
  return (x:xs)

-- Exercise 4
iforIO_ :: [t1] -> (Int -> t1 -> IO a) -> IO ()
iforIO_ [] program = return ()
iforIO_ lst program = helper 0 lst program
  where
    helper :: Int -> [t1] -> (Int -> t1 -> IO a) -> IO ()
    helper index [] program = return ()
    helper index (x:xs) program = do
      program index x
      helper (index+1) xs program

example = do
  iforIO_ [1, 2] (\i n ->
    iforIO_ "ab" (\j c ->
      print ((i, j), replicate n c)))
