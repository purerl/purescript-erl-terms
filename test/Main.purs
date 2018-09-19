module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Data.Map as Map
import Erl.Data.Tuple (tuple2)
import Erl.Terms (NativeTerm, Term(..), reflect, reify)
import Test.Assert (assert, assert')

-- Property tests would be nice here

-- Raw =:=
foreign import foreignEq :: forall a b. a -> b -> Boolean

foreign import unsafeToForeign :: forall a. a -> NativeTerm

main :: Effect Unit
main = do
  log "Test reify"
  assert $ reify (IntegerTerm 42) `foreignEq` 42
  assert $ reify (FloatTerm 1.2) `foreignEq` 1.2
  assert $ reify (AtomTerm "hello") `foreignEq` atom "hello"
  assert $ reify (ListTerm ( IntegerTerm 42 : AtomTerm "abc" : nil)) `foreignEq` 
      (unsafeToForeign 42 : unsafeToForeign (atom "abc") : nil)
  assert $ reify (TupleTerm ( IntegerTerm 42 : AtomTerm "abc" : nil)) `foreignEq` 
      (tuple2 (unsafeToForeign 42) (unsafeToForeign (atom "abc")))
  assert $ reify (MapTerm ( tuple2 (AtomTerm "a") (IntegerTerm 42)
                          : tuple2 (IntegerTerm 1) (FloatTerm 1.0)
                          : nil))
      `foreignEq` (
        Map.insert (unsafeToForeign (atom "a")) (unsafeToForeign 42) $
        Map.insert (unsafeToForeign 1) (unsafeToForeign 1.0) $
        Map.empty
      )

  log "Test reflect"
  assert $ reflect (unsafeToForeign 42) == Just (IntegerTerm 42)
  assert $ reflect (unsafeToForeign 1.2) == Just (FloatTerm 1.2)

  assert $ reflect (unsafeToForeign $ atom "hello") == Just (AtomTerm "hello")
  assert $ reflect (unsafeToForeign (unsafeToForeign 42 : unsafeToForeign (atom "abc") : nil))
    == Just (ListTerm ( IntegerTerm 42 : AtomTerm "abc" : nil))
      
  assert $ reflect (unsafeToForeign (tuple2 (unsafeToForeign 42) (unsafeToForeign (atom "abc"))))
    == Just (TupleTerm ( IntegerTerm 42 : AtomTerm "abc" : nil))

  -- TODO these tests sidestep the fact that maps don't preserve ordering

  assert $ reflect (unsafeToForeign (
        Map.insert (unsafeToForeign 1) (unsafeToForeign 1.0) $
        Map.insert (unsafeToForeign (atom "a")) (unsafeToForeign 42) $
        Map.empty
    )) == Just 
    (MapTerm  ( tuple2 (IntegerTerm 1) (FloatTerm 1.0)
              : tuple2 (AtomTerm "a") (IntegerTerm 42)
              : nil))

  log "Round trip"

  let complex = (TupleTerm
                  ( IntegerTerm 42
                  : ListTerm
                    (AtomTerm "ab"
                    : MapTerm
                      ( tuple2 (IntegerTerm 1) (FloatTerm 1.0)
                      : tuple2 (AtomTerm "a") (IntegerTerm 42)
                      : nil)
                    : nil)
                  : nil))

  assert' "does not roundtrip" $ reflect (reify complex) == Just complex
