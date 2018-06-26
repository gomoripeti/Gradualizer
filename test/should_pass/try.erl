-module('try').

-compile([export_all]).

-spec t() -> 1..3.
t()->
    try A = 1 of
        ok ->
            %%a:b(A), unsafe
            B = 3
    catch _:_ ->
            %%a:b(A), unsafe
            %%a:b(B), unbound
            C = 3
    after
        %%a:b(A), unsafe
        %%a:b(B), unsafe
        %%a:b(C), unsafe
        D = 4
    end.
    %%A. unsafe
    %% B. unsafe
    %% C. unsafe
    %% D. unsafe
