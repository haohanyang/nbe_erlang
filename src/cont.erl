% Continuation monad and delimit operators
-module(cont).

-export([run/1, bind/2, reset/1, shift/1, return/1, sequence/1]).

-export_type([m/2]).

%% Continuation monad
%% m a = (a -> term) -> term
-type cont(A, R) :: fun((fun((A) -> R)) -> R).
-type m(A, R) :: {cont, cont(A, R)}.

-spec app(m(A, R), fun((A) -> R)) -> R.
app({cont, M}, V) ->
    M(V).

-spec run(m(A, A)) -> A.
run(M = {cont, _}) ->
    app(M, fun(A) -> A end).

-spec return(A) -> m(A, A).
return(A) ->
    {cont, fun(K) -> K(A) end}.

-spec bind(m(A, R), fun((A) -> B)) -> m(B, R).
bind(T = {cont, _}, F) ->
    {cont, fun(K) -> app(T, fun(A) -> app(F(A), K) end) end}.

-spec reset(m(A, A)) -> m(A, any()).
reset(T = {cont, _}) ->
    return(run(T)).

% should formally be shift(fun((fun((A) -> R)) -> m(R, R))) -> m(A, R), with the return removed.
-spec shift(fun((fun((A) -> m(R, R))) -> m(R, R))) -> m(A, R).
shift(H) ->
    {cont, fun(K) -> run(H(fun(A) -> return(K(A)) end)) end}.

sequence([]) ->
    return([]);
sequence([M | Ms]) ->
    bind(M, fun(N) -> bind(sequence(Ms), fun(Ns) -> return([N | Ns]) end) end).
