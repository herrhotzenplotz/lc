{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Char
import qualified Data.Map.Strict as M
import Data.Version (showVersion)
import Paths_lc (version)
import System.IO (hFlush, stdout)

data Term
  = Variable String
  | Application Term Term
  | Abstraction String Term
  | Closure String Term Scope
  deriving (Show, Eq)

data InterpreterError
  = SyntaxError String
  | SemanticError String
  deriving (Show)

newtype Parser a =
  Parser
    { runParser :: String -> Either InterpreterError (String, a)
    }
  deriving (Functor)

type Scope = M.Map String Term

type InterpreterResult = Either InterpreterError Term

data InterpreterCommand
  = LetBinding String Term
  | Release String
  | EvalCommand Term

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  Parser f <*> Parser p =
    Parser $ \input -> do
      (input', f') <- f input
      (input'', res) <- p input'
      return (input'', f' res)

instance Alternative (Either InterpreterError) where
  empty = Left $ SemanticError "Empty Parser"
  Left _ <|> err = err
  ei1 <|> _ = ei1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \x -> p1 x <|> p2 x

instance Monad Parser where
  (Parser p1) >>= f =
    Parser $ \input -> do
      (input', a) <- p1 input
      runParser (f a) input'

instance MonadFail Parser where
  fail m = Parser $ const $ Left $ SyntaxError m

expectChar :: Char -> Parser ()
expectChar c =
  Parser $ \case
    c':rest
      | c' == c -> Right (rest, ())
    _ -> Left $ SyntaxError ("Expected '" <> [c] <> "'")

expectOP :: Parser ()
expectOP = expectChar '('

ws :: Parser ()
ws =
  void $
  many $
  Parser
    (\case
       x:xs
         | isSpace x -> Right (xs, ())
       _ -> Left $ SyntaxError "Expected whitespace")

expectCP :: Parser ()
expectCP = expectChar ')'

expectLambda :: Parser ()
expectLambda = expectChar '\\'

expectDot :: Parser ()
expectDot = expectChar '.'

parseIdentifier :: Parser String
parseIdentifier =
  Parser $ \inp ->
    case span isAlphaNum inp of
      ([], rest) ->
        Left $ SyntaxError ("Expected an identifier but got '" <> rest <> "'")
      (identifier, rest) -> Right (rest, identifier)

expectString :: String -> Parser ()
expectString expect = do
  got <- parseIdentifier
  if got == expect
    then return ()
    else fail $ "Expected '" <> expect <> "' bot got '" <> got <> "'"

parseApplication :: Parser Term
parseApplication = do
  expectOP
  t1 <- parseTerm
  t2 <- parseTerm
  expectCP
  return $ Application t1 t2

parseAbstraction :: Parser Term
parseAbstraction = do
  ws
  expectLambda
  (Variable boundVar) <- parseVariable
  expectDot
  Abstraction boundVar <$> parseTerm

parseVariable :: Parser Term
parseVariable = Variable <$> parseIdentifier

parseTerm :: Parser Term
parseTerm =
  ws *> (parseApplication <|> parseAbstraction <|> parseVariable) <* ws

interpretScoped :: Scope -> Term -> InterpreterResult
interpretScoped scope (Variable v) =
  case M.lookup v scope of
    Just term -> return term
    Nothing -> Left $ SemanticError ("Unbound value '" <> v <> "'")
interpretScoped scope (Abstraction arg body) = Right $ Closure arg body scope
interpretScoped scope (Application f val) = do
  fEval <- interpretScoped scope f
  case fEval of
    Closure f' body closedScope -> do
      valEval <- interpretScoped scope val
      let newScope = M.insert f' valEval closedScope
      let forEvaluation = M.union scope newScope
      interpretScoped forEvaluation body
    _ -> Left $ SemanticError "Cannot apply non-function values"
interpretScoped _ closure = return closure

printT :: Term -> String
printT (Application f val) = "(" <> printT f <> " " <> printT val <> ")"
printT (Abstraction b body) = "\\" <> b <> "." <> printT body
printT (Closure b body e) = (if M.null e then [] else ['\'']) <> "\\" <> b <> "." <> printT body
printT (Variable x) = x

printE :: InterpreterError -> String
printE (SemanticError x) = "Semantic error: " <> x
printE (SyntaxError x) = "Syntactic error: " <> x

eval :: Term -> StateT Scope IO ()
eval inp = do
  scope <- get
  case interpretScoped scope inp of
    Right result -> lift $ putStrLn $ ":: " <> printT result
    Left err -> printError err

bind :: String -> Term -> StateT Scope IO ()
bind name body = do
  globalScope <- get
  case interpretScoped globalScope body of
    Right result -> do
      put $ M.insert name result globalScope
      lift $ putStrLn $ ":: '" <> name <> "' has been bound."
    Left err -> printError err

release :: String -> StateT Scope IO ()
release binding = do
  globalScope <- get
  if M.member binding globalScope
    then do
      put $ M.delete binding globalScope
      lift $ putStrLn $ ":: '" <> binding <> "' has been released."
    else lift $ putStrLn $ "?: '" <> binding <> "': No such binding ."

printError :: InterpreterError -> StateT Scope IO ()
printError err = lift $ putStrLn $ "?: " <> printE err

parseLetBinding :: Parser InterpreterCommand
parseLetBinding = do
  ws *> expectString "let" <* ws
  binding <- ws *> parseIdentifier <* ws
  expectChar '='
  LetBinding binding <$> parseTerm

parseReleaseCommand :: Parser InterpreterCommand
parseReleaseCommand = do
  ws *> expectString "release" <* ws
  Release <$> parseIdentifier

parseEvalCommand :: Parser InterpreterCommand
parseEvalCommand = EvalCommand <$> parseTerm

parseInterpreterCommand :: Parser InterpreterCommand
parseInterpreterCommand =
  parseLetBinding <|> parseReleaseCommand <|> parseEvalCommand

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
        else lift $ putStrLn $ "Warning: Incomplete parse: " <> resStr
  repl

main :: IO ()
main = do
  putStrLn "lc - untyped lambda calculus interpreter"
  putStrLn $ "Version " <> showVersion version
  putStrLn "Copyright 2020 by Nico Sonack\n"
  fst <$> runStateT repl M.empty
