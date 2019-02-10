-module(varbind_in_block).

-export([add_vars/2]).

%% is this what we expect to happen?
-spec add_vars(1..2, 2..3) -> {{1..2, 2..3}, 2}.
add_vars(A, B) ->
    S = {V = A, V = B},
    {S, V}.
