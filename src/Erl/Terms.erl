-module(erl_terms@foreign).
-export([unsafeToForeign/1, listToTuple/1, reflectImpl/13]).

unsafeToForeign(X) -> X.

listToTuple(L) -> list_to_tuple(L).

reflectImpl(Int,Float,Atom,Binary,Pid,Reference,List,Tuple,Map,Nothing,Just,ReflectList,E) ->
    if
        is_integer(E) -> Just(Int(E));
        is_float(E) -> Just(Float(E));
        is_atom(E) -> Just(Atom(atom_to_binary(E, utf8)));
        is_pid(E) -> Just(Pid(E));
        is_reference(E) -> Just(Reference(E));
        is_list(E) -> (ReflectList(List))(E);
        is_tuple(E) -> 
            L = erlang:tuple_to_list(E),
            (ReflectList(Tuple))(L);
        is_binary(E) -> Just(Binary(E));
        is_map(E) -> 
            L = maps:to_list(E),
            % Extract tuple of term (rep)s from TupleTerm 
            ExtractTuple = fun({_, [T1, T2]}) -> {T1, T2} end,
            F = fun (ResL) -> Map(lists:map(ExtractTuple, ResL)) end,
            (ReflectList(F))(L);
        true -> Nothing
    end.