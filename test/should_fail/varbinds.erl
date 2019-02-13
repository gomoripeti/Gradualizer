-module(varbinds).

-export([f/2, g/2]).

%% add_any_type_pat should not override existing varbind for I
-spec f(integer(), any()) -> float().
f(I, Any) ->
    I = Any,
    I.

%% varbinds should be propagated from right to left side of match
-spec g(1, 2) -> integer().
g(One, Two) ->
    {1, X} = {(X = One), Two},
    X.
