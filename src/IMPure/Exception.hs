module IMPure.Exception where

import Control.Exception (Exception)
import Text.Printf (printf)

data Result a = Legal a | Error InterpreterException

instance Functor Result where
  fmap f (Legal r) = Legal (f r)
  fmap _ (Error e) = Error e

instance Applicative Result where
  pure r = Legal r
  (<*>) (Legal f) (Legal r) = Legal (f r)
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e

data InterpreterException
  = UndeclearedVariable String
  | MultipleDeclaration String
  | InvalidBExp String
  | InvalidAExp String

instance Show InterpreterException where
  show (UndeclearedVariable s) = printf "Undecleared variable! '%s'" s
  show (MultipleDeclaration s) = printf "Multiple declaration of a variable! '%s'" s
  show (InvalidBExp s) = printf "The boolean expression is invalid! '%s'" s
  show (InvalidAExp s) = printf "The arithmetic expression is invalid! '%s'" s

instance Exception InterpreterException
