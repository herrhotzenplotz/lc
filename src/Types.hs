module Types where

import qualified Data.Map.Strict as M

data Term
  = Variable String
  | Application Term Term
  | Abstraction String Term
  | Closure String Term Scope
  deriving (Show, Eq)

type Scope = M.Map String Term
