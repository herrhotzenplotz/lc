module Main where

import Error
import Interpreter
import Parser
import Types

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Data.Version (showVersion)
import Paths_lc (version)
import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , setSGR
  )
import System.IO (hFlush, stdout)

repl :: StateT Scope IO ()
repl = do
  lift $ putStr "> "
  lift $ hFlush stdout
  inp <- lift getLine
  let p = runParser parseInterpreterCommand inp
  case p of
    Left err -> printError err
    Right (resStr, command) -> do
      case command of
        EvalCommand term -> eval term
        LetBinding binding term -> bind binding term
        Release binding -> release binding
      if null resStr
        then return ()
        else lift $ do
               setSGR [SetColor Foreground Vivid Magenta]
               putStrLn $ "Warning: Incomplete parse: " <> resStr
               setSGR []
  repl

main :: IO ()
main = do
  putStrLn "lc - untyped lambda calculus interpreter"
  putStrLn $ "Version " <> showVersion version
  putStrLn "Copyright 2020 by Nico Sonack\n"
  fst <$> runStateT repl M.empty
