module Types where

import qualified Data.Map as M
import Data.Monoid

data TokenPosition =
  TokenPosition
    { positionFile :: String
    , positionLine :: Int
    , positionColumn :: Int
    }
  deriving (Eq)

instance Show TokenPosition where
  show (TokenPosition file line col) =
    file <> ":" <> show line <> ":" <> show col

data Term
  = Variable String TokenPosition
  | Application Term Term TokenPosition
  | Abstraction String Term TokenPosition
  | Closure String Term Scope TokenPosition
  deriving (Show, Eq)

type Scope = M.Map String Term
