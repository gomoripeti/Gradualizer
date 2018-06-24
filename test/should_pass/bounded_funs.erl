-module(bounded_funs).

-export([f/0, h/0, funs/0]).

-spec f() -> term().
f() ->
    g_list([1, 2]),
    g_any({1, 2}),
    g(myatom).

-spec g(Atom) -> any() when Atom :: atom().
g(A) ->
    A.

-spec g_list(List) -> any() when List :: list().
g_list(L) ->
    L.

-spec g_any(Any) -> any() when Any :: term().
g_any(A) ->
    A.

h() ->
    ets:lookup_element(myatom, asd, 2).

funs() ->
    F1 = fun g/1,
    F2 = fun ets:lookup_element/3,
    {F1, F2}.
