module Interpreter where

import Error
import Parser
import Types

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , setSGR
  )

type InterpreterResult = Either InterpreterError Term

data InterpreterCommand
  = LetBinding String Term
  | Release String
  | EvalCommand Term

parseApplication :: Parser Term
parseApplication =
  parenthesized $ do
    t1 <- parseTerm
    t2 <- parseTerm
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
  parseReleaseCommand <|> parseLetBinding <|> parseEvalCommand

showTerm :: Term -> StateT Scope IO String
showTerm (Application f val) = do
  f' <- showTerm f
  val' <- showTerm val
  return $ "(" <> f' <> " " <> val' <> ")"
showTerm (Abstraction b body) = do
  body' <- showTerm body
  return $ "\\" <> b <> "." <> body'
showTerm (Closure b body e) = do
  globalScope <- get
  body' <- showTerm body
  return $
    (['\'' | not (M.null (e M.\\ globalScope))]) <> "\\" <> b <> "." <> body'
showTerm (Variable x) = return x

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

eval :: Term -> StateT Scope IO ()
eval inp = do
  scope <- get
  case interpretScoped scope inp of
    Right result -> do
      result' <- showTerm result
      lift $ putStrLn $ ":: " <> result'
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
    else lift $ do
           setSGR [SetColor Foreground Vivid Red]
           putStrLn $ "?: '" <> binding <> "': No such binding."
           setSGR []
