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

data InterpreterError
  = SyntaxError String
  | SemanticError String
  deriving (Show)

instance Alternative (Either InterpreterError) where
  empty = Left $ SemanticError "Empty error"
  Left _ <|> err = err
  ei1 <|> _ = ei1

printError :: InterpreterError -> StateT Scope IO ()
printError err =
  lift $ do
    setSGR [SetColor Foreground Vivid Red]
    case err of
      SemanticError x -> do
        putStr "?: Semantic error: "
        setSGR []
        putStrLn x
      SyntaxError x -> do
        putStr "?: Syntactic error: "
        setSGR []
        putStrLn x
