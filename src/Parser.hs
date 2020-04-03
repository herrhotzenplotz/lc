{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Error

newtype Parser a =
  Parser
    { runParser :: String -> Either InterpreterError (String, a)
    }
  deriving (Functor)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  Parser f <*> Parser p =
    Parser $ \input -> do
      (input', f') <- f input
      (input'', res) <- p input'
      return (input'', f' res)

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
parenthesized :: Parser a -> Parser a
parenthesized p = expectOP *> p <* expectCP

expectString :: String -> Parser ()
expectString expect = do
  got <- parseIdentifier
  if got == expect
    then return ()
    else fail $ "Expected '" <> expect <> "' bot got '" <> got <> "'"
