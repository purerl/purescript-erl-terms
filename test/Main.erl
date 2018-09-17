-module(test_main@foreign).
-export([foreignEq/2]).

foreignEq(X, Y) -> X =:= Y.