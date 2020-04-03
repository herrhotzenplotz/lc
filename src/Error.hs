{-# LANGUAGE FlexibleInstances #-}

module Error where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , setSGR
  )
import Types

data ErrorMessage =
  ErrorMessage
    { errorMessageText :: String
    , errorMessageFile :: String
    , errorMessageLine :: Int
    , errorMessageColumn :: Int
    }

instance Show ErrorMessage where
    show (ErrorMessage msg file line col) = file <> ":" <> (show line) <> ":" <> (show col) <> ": " <> msg

data InterpreterError
  = SyntaxError ErrorMessage
  | SemanticError String
  | InternalError String
  deriving (Show)

instance Alternative (Either InterpreterError) where
  empty = Left $ SyntaxError $ ErrorMessage "Empty error" "no-file" 0 0
  Left _ <|> err = err
  ei1 <|> _ = ei1

printError :: InterpreterError -> StateT Scope IO ()
printError err =
  lift $ do
    setSGR [SetColor Foreground Vivid Red]
    case err of
      SemanticError x -> do
        putStrLn $ show x
      SyntaxError x -> do
        putStrLn $ show x
      InternalError msg ->
        putStr $ "?: Internal interpreter error: " <> msg
    setSGR []
