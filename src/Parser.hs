{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Error
import Types

data ParserInputStream =
  ParserInputStream
    { streamPosition :: Int
    , streamLine :: Int
    , streamBegin :: String
    , streamFileName :: String
    }

newtype Parser a =
  Parser
    { runParser :: ParserInputStream -> Either InterpreterError ( ParserInputStream
                                                                , a)
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
  fail msg = Parser $ const $ Left $ InternalError msg

getPosition :: Parser TokenPosition
getPosition =
  Parser $ \input ->
    Right
      ( input
      , TokenPosition
          (streamFileName input)
          (streamLine input)
          (streamPosition input))

failedParser :: String -> Parser a
failedParser msg =
  Parser $ \(ParserInputStream pos line _ fileName) ->
    Left $ SyntaxError $ ErrorMessage msg $ TokenPosition fileName line pos

expectChar :: Char -> Parser ()
expectChar c =
  Parser $ \case
    (ParserInputStream pos line (c':rest) fileName)
      | c' == c -> Right (ParserInputStream (pos + 1) line rest fileName, ())
    (ParserInputStream pos line _ fileName) ->
      Left $
      SyntaxError $
      ErrorMessage ("Expected '" <> [c] <> "'") $
      TokenPosition fileName line pos

expectOP :: Parser ()
expectOP = expectChar '('

eof :: Parser ()
eof =
  Parser $ \case
    stream@(ParserInputStream _ _ [] _) -> Right (stream, ())
    (ParserInputStream pos line rest fileName) ->
      Left $
      SyntaxError $
      ErrorMessage ("Expected EOF but got '" <> rest <> "' instead") $
      TokenPosition fileName line pos

ws :: Parser ()
ws =
  void $
  many $
  Parser
    (\case
       ParserInputStream pos line (x:xs) fileName
         | isSpace x -> Right (ParserInputStream (pos + 1) line xs fileName, ())
       ParserInputStream pos line _ fileName ->
         Left $
         SyntaxError $
         ErrorMessage "Expected whitespace" $ TokenPosition fileName line pos)

expectCP :: Parser ()
expectCP = expectChar ')'

expectLambda :: Parser ()
expectLambda = expectChar '\\'

expectDot :: Parser ()
expectDot = expectChar '.'

parseIdentifier :: Parser String
parseIdentifier =
  Parser $ \inp ->
    let ParserInputStream pos line streamHead fileName = inp
     in case span isAlphaNum streamHead of
          ([], rest) ->
            Left $
            SyntaxError $
            ErrorMessage ("Expected an identifier but got '" <> rest <> "'") $
            TokenPosition fileName line pos
          (identifier, rest) ->
            Right
              ( ParserInputStream (pos + length identifier) line rest fileName
              , identifier)

parenthesized :: Parser a -> Parser a
parenthesized p = expectOP *> p <* expectCP

expectString :: String -> Parser ()
expectString expect = do
  got <- parseIdentifier
  if got == expect
    then return ()
    else failedParser $ "Expected '" <> expect <> "' bot got '" <> got <> "'"
