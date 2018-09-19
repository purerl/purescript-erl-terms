-module(test_main@foreign).
-export([foreignEq/2, unsafeToForeign/1]).

unsafeToForeign(X) -> X.

foreignEq(X, Y) -> X =:= Y.