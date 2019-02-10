-module(named_fun).

-export([bar/0
         %%,f/0
        ]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo(fun F(0) -> 0; F(X) -> F(X - 1) end).

%% only fails with --infer
%% f() ->
%%     FF = fun F(0) -> 0; F(X) -> F(X - 1) end,
%%     F = 2,
%%     FF = 1,
%%     {F, FF}.
            
