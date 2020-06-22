module Interpreter where

import Error
import Parser
import Types

import Control.Applicative
import Control.Monad.Trans (lift)
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid

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
    Application t1 t2 <$> getPosition

parseAbstraction :: Parser Term
parseAbstraction = do
  ws
  expectLambda
  (Variable boundVar pos) <- parseVariable
  expectDot
  t <- parseTerm
  return $ Abstraction boundVar t pos

parseVariable :: Parser Term
parseVariable = do
  pos <- getPosition
  ident <- parseIdentifier
  return $ Variable ident pos

parseTerm :: Parser Term
parseTerm =
  ws *> (parseApplication <|> parseAbstraction <|> parseVariable) <* ws

parseLine :: Parser Term
parseLine = parseTerm <* eof

parseLetBinding :: Parser InterpreterCommand
parseLetBinding = do
  ws *> expectString "let" <* ws
  binding <- ws *> parseIdentifier <* ws
  expectChar '='
  LetBinding binding <$> parseLine

parseReleaseCommand :: Parser InterpreterCommand
parseReleaseCommand = do
  ws *> expectString "release" <* ws
  Release <$> parseIdentifier

parseEvalCommand :: Parser InterpreterCommand
parseEvalCommand = EvalCommand <$> (parseLine <* eof)

parseInterpreterCommand :: Parser InterpreterCommand
parseInterpreterCommand =
  parseReleaseCommand <|> parseLetBinding <|> parseEvalCommand

showTerm :: Term -> StateT Scope IO String
showTerm (Application f val _) = do
  f' <- showTerm f
  val' <- showTerm val
  return $ "(" <> f' <> " " <> val' <> ")"
showTerm (Abstraction b body _) = do
  body' <- showTerm body
  return $ "\\" <> b <> "." <> body'
showTerm (Closure b body e _) = do
  globalScope <- get
  body' <- showTerm body
  return $
    (['\'' | not (M.null (e M.\\ globalScope))]) <> "\\" <> b <> "." <> body'
showTerm (Variable x _) = return x

interpretScoped :: Scope -> Term -> InterpreterResult
interpretScoped scope (Variable v pos) =
  case M.lookup v scope of
    Just term -> return term
    Nothing ->
      Left $ SemanticError $ ErrorMessage ("Unbound value '" <> v <> "'") pos
interpretScoped scope (Abstraction arg body pos) =
  Right $ Closure arg body scope pos
interpretScoped scope (Application f val pos) = do
  fEval <- interpretScoped scope f
  case fEval of
    Closure f' body closedScope _ -> do
      valEval <- interpretScoped scope val
      let newScope = M.insert f' valEval closedScope -- \x.x 2 (valEval : 2, f' : "x", body : "x")
      let forEvaluation = M.union scope newScope
      interpretScoped forEvaluation body
    _ ->
      Left $ SemanticError $ ErrorMessage "Cannot apply non-function values" pos
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
    else lift $ putStrLn $ "?: '" <> binding <> "': No such binding."
