module Puzzle
  (runGame, freshPuzzle)
  where

import Data.List (intersperse)
import System.Exit (exitSuccess)
import Data.Maybe (isJust)
import Control.Monad (forever)

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed misses) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
    ++ "; Incorrect guesses: " ++ show misses

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing  = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (fmap (const Nothing) word) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle secretWord _ _ _) c = elem c secretWord

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter puzzle@(Puzzle word filledInSoFar s misses) c =
  Puzzle word newFilledInSoFar (c : s) newMisses
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar
        newMisses = if charInWord puzzle c
                    then misses
                    else misses + 1

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character,\
               \ pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word,\
               \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ misses) =
  if misses > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin  puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                    \ be a single character"

