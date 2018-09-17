module Terms where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (foldl, traverse)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, uncurry2)

foreign import data Foreign :: Type

foreign import unsafeToForeign :: forall a. a -> Foreign

foreign import listToTuple :: forall a. List a -> Foreign

data Term =
  IntegerTerm Int
  | FloatTerm Number
  | AtomTerm String
  | BinaryTerm Binary
  | ListTerm (List Term)
  | TupleTerm (List Term)
  | MapTerm (List (Tuple2 Term Term))

derive instance eqTerm :: Eq Term

reify :: Term -> Foreign
reify (IntegerTerm n) = unsafeToForeign n
reify (FloatTerm f) = unsafeToForeign f
reify (AtomTerm a) = unsafeToForeign (atom a)
reify (BinaryTerm b) = unsafeToForeign b
reify (ListTerm lt) = unsafeToForeign $ reify <$> lt
reify (TupleTerm lt) = listToTuple $ reify <$> lt
reify (MapTerm ts) = unsafeToForeign $ 
  foldl (\m t -> uncurry2 (\a b -> Map.insert (reify a) (reify b) m) t) Map.empty ts

foreign import reflectImpl :: (Int -> Term) -> (Number -> Term) -> (String -> Term) -> (Binary -> Term) -> (List Term -> Term) -> (List Term -> Term) ->  (List (Tuple2 Term Term) -> Term) -> Maybe Term -> (Term -> Maybe Term) -> ((List Term -> Term) -> List Foreign -> Maybe Term) -> Foreign -> Maybe Term

reflect :: Foreign -> Maybe Term
reflect term = reflectImpl IntegerTerm FloatTerm AtomTerm BinaryTerm ListTerm TupleTerm MapTerm Nothing Just 
  (\ctor terms -> ctor <$> traverse reflect terms)
  term
