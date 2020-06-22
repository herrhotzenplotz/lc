{-# LANGUAGE FlexibleInstances #-}

module Error where

import Control.Applicative (Applicative(..),Alternative(..))
import Control.Monad.Trans(lift)
import Control.Monad.State
import Types
import Data.Monoid

data ErrorMessage =
  ErrorMessage
    { errorMessageText :: String
    , errorPosition :: TokenPosition
    }

instance Show ErrorMessage where
  show (ErrorMessage msg pos) = show pos <> ": Error: " <> msg

printPositionIndicator :: ErrorMessage -> IO ()
printPositionIndicator errorMsg =
  putStrLn $
  "~" <> replicate (positionColumn $ errorPosition errorMsg) '~' <> "^"

-- v--- this depends on the length of the prompt
data InterpreterError
  = SyntaxError ErrorMessage
  | SemanticError ErrorMessage
  | InternalError String
  deriving (Show)

instance Applicative (Either InterpreterError) where
  pure = Right

instance Alternative (Either InterpreterError) where
  empty =
    Left $
    SyntaxError $ ErrorMessage "Empty error" $ TokenPosition "no-file" 0 0
  Left _ <|> err = err
  ei1 <|> _ = ei1

printError :: InterpreterError -> StateT Scope IO ()
printError err =
  lift $ do
    case err of
      SemanticError x -> printFormattedError x
      SyntaxError x -> printFormattedError x
      InternalError msg -> putStr $ "?: Internal interpreter error: " <> msg
  where
    printFormattedError :: ErrorMessage -> IO ()
    printFormattedError x = do
      printPositionIndicator x
      print x
