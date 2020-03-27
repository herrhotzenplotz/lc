{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as M
import Data.Version (showVersion)
import System.IO (hFlush, stdout)
import Paths_lc (version)

data Token
    = Identifier String
    | Lambda
    | OpenParen
    | CloseParen
    | Dot
      deriving (Show, Eq)

data Term
    = Variable String
    | Application Term Term
    | Abstraction String Term
    | Closure String Term Scope
      deriving (Show, Eq)

data InterpreterError
    = SyntaxError String
    | SemanticError String
      deriving Show

newtype Parser a
    = Parser {
        runParser :: [Token] -> Either InterpreterError ([Token], a)
      } deriving Functor

type Scope = M.Map String Term

instance Applicative Parser where
    pure x = Parser $ \input -> Right (input, x)
    Parser f <*> Parser p = Parser $ \input -> do
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
    (Parser p1) >>= f = Parser $ \input -> do
                          (input', a) <- p1 input
                          runParser (f a) input'

instance MonadFail Parser where
    fail m = Parser $ const $ Left $ SemanticError m

tokenize :: String -> Either InterpreterError [Token]
tokenize input = case tokenizeSingle input of
                   Left e -> Left e
                   Right (token, []) -> Right $ return token
                   Right (token, rest) -> do
                     parsedRest <- tokenize rest
                     return $ token : parsedRest
    where tokenizeSingle :: String -> Either InterpreterError (Token, String)
          tokenizeSingle (x : rest) | isSpace x = tokenizeSingle rest
          tokenizeSingle ('\\' : rest) = return (Lambda, rest)
          tokenizeSingle ('.' : rest) = return (Dot, rest)
          tokenizeSingle ('(' : rest) = return (OpenParen, rest)
          tokenizeSingle (')' : rest) = return (CloseParen, rest)
          tokenizeSingle maybeString = case span isAlphaNum maybeString of
                                         ([], []) -> Left $ SyntaxError "Unexpected EOF"
                                         ([], gibberish) -> Left $ SyntaxError ("Unexpected '" <> [head gibberish, '\''])
                                         (token, rest) -> return (Identifier token, rest)

parseVariable :: Parser Term
parseVariable = Parser $ \case
                  (Identifier x):restToks -> Right (restToks, Variable x)
                  h:_ -> Left $ SemanticError ("Unexpected " <> show h)
                  [] -> Left $ SemanticError "Expected variable"

expectOP :: Parser ()
expectOP = Parser $ \case
           OpenParen:restToks -> Right (restToks, ())
           h:_ -> Left $ SyntaxError ("Expected '(' but got '" <> show h <> "' instead")
           [] -> Left $ SemanticError "Expected '('"

expectCP :: Parser ()
expectCP = Parser $ \case
           CloseParen:restToks -> Right (restToks, ())
           h:_ -> Left $ SyntaxError ("Expected ')' but got '" <> show h <> "' instead")
           [] -> Left $ SemanticError "Expected ')'"

expectDot :: Parser ()
expectDot = Parser $ \case
            Dot:restToks -> Right (restToks, ())
            h:_ -> Left $ SyntaxError ("Expected '.' but got '" <> show h <> "' instead")
            [] -> Left $ SemanticError "Expected '.'"

expectLambda :: Parser ()
expectLambda = Parser $ \case
               Lambda:restToks -> Right (restToks, ())
               h:_ -> Left $ SyntaxError ("Expected '\\' but got '" <> show h <> "' instead")
               [] -> Left $ SemanticError "Expected '\\'"


parseApplication :: Parser Term
parseApplication = do
  expectOP
  t1 <- parseTerm
  t2 <- parseTerm
  expectCP
  return $ Application t1 t2

parseAbstraction :: Parser Term
parseAbstraction = do
  expectLambda
  (Variable boundVar) <- parseVariable
  expectDot
  Abstraction boundVar <$> parseTerm

parseTerm :: Parser Term
parseTerm = parseApplication <|> parseAbstraction <|> parseVariable

type InterpreterResult = Either InterpreterError Term

interpretScoped :: Scope -> Term -> InterpreterResult
interpretScoped scope (Variable v) = case M.lookup v scope of
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

interpret :: Term -> InterpreterResult
interpret = interpretScoped M.empty

printT :: Term -> String
printT (Application f val) = "(" <> printT f <> " " <> printT val <> ")"
printT (Abstraction b body) = "\\" <> b <> "." <> printT body
printT (Closure b body _) = "\\" <> b <> "." <> printT body
printT (Variable x) = x

printE :: InterpreterError -> String
printE (SemanticError x) = "Semantic error: " <> x
printE (SyntaxError x) = "Syntactic error: " <> x

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  inp <- getLine
  let r = do
         toks <- tokenize inp
         parseRes <- runParser parseTerm toks
         interpret $ snd parseRes
  case r of
    Right result -> putStrLn $ ":: " <> printT result
    Left err -> putStrLn $ "?: " <> printE err
  repl

main :: IO ()
main = do
  putStrLn "lc - untyped lambda calculus interpreter"
  putStrLn $ "Version " <> showVersion version
  putStrLn "Copyright 2020 by Nico Sonack\n"
  repl
