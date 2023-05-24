-module(naive).

-compile(export_all).

-type syn() ::
    {'LAM', atom(), syn()}
    | {'APP', syn(), syn()}
    | {'PAIR', syn(), syn()}
    | {'FST', syn()}
    | {'SND', syn()}
    | {'VAR', atom()}.
-type sem() :: {'lam', fun((sem()) -> sem())} | {'pair', {sem(), sem()}} | {'syn', syn()}.
-type typ() :: {'ty_fun', typ(), typ()} | {'ty_pair', typ(), typ()} | ty_base.

-type ctx() :: #{atom() => sem()}.

-spec eval(syn(), ctx()) -> sem().

eval({'LAM', Arg, Body}, Ctx) ->
    {'lam', fun(X) -> eval(Body, maps:put(Arg, X, Ctx)) end};
eval({'APP', FUN, X}, Ctx) ->
    case eval(FUN, Ctx) of
        {'lam', Fun} -> Fun(eval(X, Ctx));
        _ -> erlang:error(badarg)
    end;
eval({'PAIR', T1, T2}, Ctx) ->
    {'pair', {eval(T1, Ctx), eval(T2, Ctx)}};
eval({'FST', T}, Ctx) ->
    case eval(T, Ctx) of
        {'pair', Pair} -> element(1, Pair);
        _ -> erlang:error(badarg)
    end;
eval({'SND', T}, Ctx) ->
    case eval(T, Ctx) of
        {'pair', Pair} -> element(2, Pair);
        _ -> erlang:error(badarg)
    end;
eval({'VAR', At}, Ctx) ->
    maps:get(At, Ctx).

-spec reify(sem(), typ()) -> syn().
reify({'lam', Fun}, {'ty_fun', A, B}) ->
    Var = aux_server:new_var(),
    {'LAM', Var, reify(Fun(reflect({'VAR', Var}, A)), B)};
reify({'pair', Pair}, {'ty_pair', T1, T2}) ->
    {'PAIR', reify(element(1, Pair), T1), reify(element(2, Pair), T2)};
reify({'syn', Syn}, ty_base) ->
    Syn.

-spec reflect(term(), typ()) -> sem().
reflect(Term, {'ty_fun', A, B}) ->
    {'lam', fun(X) -> reflect({'APP', Term, (reify(X, A))}, B) end};
reflect(Term, {'ty_pair', {T1, T2}}) ->
    {'pair', {reflect({'FST', Term}, T1), reflect({'SND', Term}, T2)}};
reflect(Term, 'ty_base') ->
    {'syn', Term}.

k() -> {'LAM', x, {'LAM', y, {'VAR', x}}}.
s() ->
    {'LAM', x,
        {'LAM', y,
            {'LAM', z, {'APP', {'APP', {'VAR', x}, {'VAR', z}}, {'APP', {'VAR', y}, {'VAR', z}}}}}}.
skk() -> {'APP', {'APP', s(), k()}, k()}.

norm(Term, Type) ->
    aux_server:start(),
    reify(eval(Term, maps:new()), Type).
