module Erl.Data.Reference where

import Prelude
import Effect (Effect)

foreign import data Reference :: Type

foreign import makeRef :: Effect Reference

foreign import eqRef :: Reference -> Reference -> Boolean

instance eqReference :: Eq Reference where
  eq = eqRef