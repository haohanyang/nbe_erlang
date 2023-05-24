-module(test_cont).

-import(cont, [shift/1, bind/2, reset/1, return/1, run/1]).

-include_lib("eunit/include/eunit.hrl").

cont_1_test() -> 
    % reset (shift(λk.k 3 *ᶜ λx.ηᶜ (x + 1)) *ᶜ λr.ηᶜ (2 × r)) *ᶜ λa.ηᶜ (−a)
    S1 = shift(fun(K) -> bind(K(3), fun(X) -> return(X + 1) end) end),
    S2 = bind(S1, fun(R) -> return(2 * R) end),
    S3 = bind(reset(S2), fun(A) -> return(-A) end),
    Res = run(S3),
    ?assertEqual(-7, Res).
