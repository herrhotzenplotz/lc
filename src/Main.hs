module Main where

import Error
import Interpreter
import Parser
import Types

import Control.Applicative
import Control.Monad
import Control.Monad.Trans(lift)
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import Data.Version (showVersion)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

runLine :: FilePath -> Int -> String -> StateT Scope IO ()
runLine path line inp =
  case runParser parseInterpreterCommand $ ParserInputStream 1 line inp path of
    Left err -> printError err
    Right (resStr, command) -> do
      case command of
        EvalCommand term -> eval term
        LetBinding binding term -> bind binding term
        Release binding -> release binding
      if null $ streamBegin resStr
        then return ()
        else lift $ do
               putStrLn $
                 path <>
                 ":" <>
                 show (streamPosition resStr) <>
                 ": Warning: Incomplete parse: " <> streamBegin resStr

repl :: StateT Scope IO ()
repl = do
  lift $ putStr "> "
  lift $ hFlush stdout
  input <- lift getLine
  case input of
    [] -> return ()
    someInput -> runLine "<interactive>" 0 someInput
  repl

interpretFile :: FilePath -> StateT Scope IO ()
interpretFile filePath = do
  lift $ putStrLn $ "Loading file '" <> filePath <> "'"
  code <- lift $ zip [1 ..] . lines <$> readFile filePath
  forM_ code $ \line -> do
    lift $ putStrLn $ ">>" <> snd line
    uncurry (runLine filePath) line

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
  (_, replState) <- runStateT (mapM_ interpretFile args) M.empty
  fst <$> runStateT repl replState

main :: IO ()
main = do
  putStrLn "lc - untyped lambda calculus interpreter"
  putStrLn $ "Version 1.0 for Hugs"
  putStrLn "Copyright 2020 by Nico Sonack\n"
  getArgs >>= mainWithArgs
