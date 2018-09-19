module Erl.Terms (
  Term(..)
, NativeTerm
, reify
, reflect
) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (foldl, traverse)
import Erl.Atom (atom)
import Erl.Data.Bitstring (Bitstring)
import Erl.Data.List (List)
import Erl.Data.Map as Map
import Erl.Data.Reference (Reference)
import Erl.Data.Tuple (Tuple2, uncurry2)
import Erl.Process.Raw (Pid)

foreign import data NativeTerm :: Type

foreign import unsafeToForeign :: forall a. a -> NativeTerm

foreign import listToTuple :: forall a. List a -> NativeTerm

data Term =
  IntegerTerm Int
  | FloatTerm Number
  | AtomTerm String
  | BitstringTerm Bitstring
  | ListTerm (List Term)
  | TupleTerm (List Term)
  | MapTerm (List (Tuple2 Term Term))
  | PidTerm Pid
  | ReferenceTerm Reference

derive instance eqTerm :: Eq Term

reify :: Term -> NativeTerm
reify (IntegerTerm n) = unsafeToForeign n
reify (FloatTerm f) = unsafeToForeign f
reify (AtomTerm a) = unsafeToForeign (atom a)
reify (BitstringTerm b) = unsafeToForeign b
reify (ListTerm lt) = unsafeToForeign $ reify <$> lt
reify (TupleTerm lt) = listToTuple $ reify <$> lt
reify (MapTerm ts) = unsafeToForeign $ 
  foldl (\m t -> uncurry2 (\a b -> Map.insert (reify a) (reify b) m) t) Map.empty ts
reify (PidTerm pid) = unsafeToForeign pid
reify (ReferenceTerm ref) = unsafeToForeign ref

foreign import reflectImpl :: (Int -> Term) -> (Number -> Term) -> (String -> Term) -> (Bitstring -> Term) -> (Pid -> Term) -> (Reference -> Term)
  -- ListTerm               TupleTerm               MapTerm
  -> (List Term -> Term) -> (List Term -> Term) ->  (List (Tuple2 Term Term) -> Term)
  -- Nothing       Just
  -> Maybe Term -> (Term -> Maybe Term)
  -- reflect list
  -> ((List Term -> Term) -> List NativeTerm -> Maybe Term)
  -> NativeTerm
  -> Maybe Term

reflect :: NativeTerm -> Maybe Term
reflect term = reflectImpl IntegerTerm FloatTerm AtomTerm BitstringTerm PidTerm ReferenceTerm ListTerm TupleTerm MapTerm Nothing Just 
  (\ctor terms -> ctor <$> traverse reflect terms)
  term
