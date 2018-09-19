-module(erl_data_reference@foreign).
-export([makeRef/0, eqRef/2]).

makeRef() -> fun () ->
    erlang:make_ref()
end.

eqRef(X,Y) -> X =:= Y.