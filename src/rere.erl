-module(rere).

-type expr() :: erl_parse:abstract_expr().
-type tp() :: nbe:tp().
-type sem() :: nbe:sem().

-type m(A, R) :: {rere, fun((integer()) -> cont:m({integer(), A}, R))}.

-export([bind/2, return/1, run/1]).
-export([reify/2, reflect/2, call/3]).

run(A = {rere, _}) ->
    cont:run(
        cont:bind(
            run(1, A),
            fun({_, B}) -> cont:return(B) end
        )
    ).

run(S, {rere, M}) -> M(S).

return(X) ->
    {rere, fun(S) ->
        cont:return({S, X})
    end}.
bind(M = {rere, _}, K) ->
    {rere, fun(S) ->
        cont:bind(
            run(S, M),
            fun({S1, A}) ->
                run(S1, K(A))
            end
        )
    end}.

reset(T = {rere, _}) ->
    {rere, fun(S) ->
        cont:reset(run(S, T))
    end}.
shift(H) ->
    {rere, fun(S) ->
        cont:shift(fun(K) ->
            run(
                S,
                H(fun(A) ->
                    {rere, fun(S1) -> K({S1, A}) end}
                end)
            )
        end)
    end}.
shift_branch(H) ->
    {rere, fun(S) ->
        cont:shift(fun(K) ->
            run(
                S,
                H(fun(A) ->
                    {rere, fun(_) -> K({S, A}) end}
                end)
            )
        end)
    end}.

sequence([]) ->
    return([]);
sequence([M | Ms]) ->
    bind(M, fun(N) -> bind(sequence(Ms), fun(Ns) -> return([N | Ns]) end) end).

new_var() ->
    {rere, fun(N) ->
        V = list_to_atom("V" ++ integer_to_list(N)),
        cont:return({N + 1, ast_utils:make_var(V)})
    end}.
new_vars(Ts) -> sequence([new_var() || _ <- Ts]).

-spec reify(tp(), sem()) -> m([expr()], [expr()]).
reify({atom, _, At}, {sem_atom, At}) ->
    return([ast_utils:make_at(At)]);
reify(_, {sem_var, Var}) ->
    return([Var]);
reify({type, _, integer, []}, {sem_int, Int}) ->
    return([ast_utils:make_int(Int)]);
reify({type, _, string, []}, {sem_str, Str}) ->
    return([ast_utils:make_str(Str)]);
reify({type, _, atom, []}, {sem_atom, At}) ->
    return([ast_utils:make_at(At)]);
reify({type, _, boolean, []}, {sem_atom, At}) ->
    return([ast_utils:make_at(At)]);
reify({type, _, 'fun', [{type, _, product, Ts}, T2]}, {sem_fun, F}) ->
    bind(
        reflect_seq(
            Ts,
            fun(X) -> bind(F(X), fun(B) -> reify(T2, B) end) end
        ),
        fun({Vars, E}) ->
            return([ast_utils:make_fun(Vars, E)])
        end
    );
reify({type, _, tuple, Ts}, {sem_tuple, As}) ->
    bind(
        reify_seq(Ts, As),
        fun(Vs) ->
            return([ast_utils:make_tuple(Vs)])
        end
    );
reify(
    {type, _, union, [T0 = {atom, _, Tag} | _]},
    V0 = {sem_atom, Tag}
) ->
    reify(T0, V0);
reify(
    {type, _, union, [T0 = {type, _, tuple, [{atom, _, Tag} | _]} | _]},
    V0 = {sem_tuple, [{sem_atom, Tag} | _]}
) ->
    reify(T0, V0);
reify({type, ANNO, union, [_ | Rest]}, V) ->
    reify({type, ANNO, union, Rest}, V).

-spec reify_seq([tp()], [sem()]) -> m([expr()], [expr()]).
reify_seq(Ts, As) ->
    sequence(lists:zipwith(fun reify_one/2, Ts, As)).

-spec reify_one(tp(), sem()) -> m(expr(), [expr()]).
reify_one(T, A) ->
    bind(
        reify(T, A),
        fun(X) ->
            shift(fun(K) ->
                bind(
                    K(lists:last(X)),
                    fun(V) -> return(lists:droplast(X) ++ V) end
                )
            end)
        end
    ).

-spec reflect(tp(), atom()) -> m(sem(), [expr()]).
reflect({atom, _, At}, _) ->
    return({sem_atom, At});
reflect({type, _, integer, []}, At) ->
    return({sem_var, At});
reflect({type, _, string, []}, At) ->
    return({sem_var, At});
reflect({type, _, atom, []}, At) ->
    return({sem_var, At});
reflect({type, _, boolean, []}, At) ->
    shift_branch(fun(K) ->
        bind(
            sequence([
                K({sem_atom, true}),
                K({sem_atom, false})
            ]),
            fun
                ([E, E]) -> return(E);
                ([E1, E2]) -> return([ast_utils:make_if_else(At, E1, E2)])
            end
        )
    end);
reflect(Type = {type, _, 'fun', _}, At) ->
    return({sem_fun, fun(Sems) -> call(Type, At, Sems) end});
reflect({type, _, tuple, Types}, At) ->
    shift(fun(K) ->
        bind(
            reflect_seq(Types, fun(Sems) -> K({sem_tuple, Sems}) end),
            fun({Vars, Rest}) ->
                return([
                    ast_utils:make_match(ast_utils:make_tuple(Vars), At)
                    | Rest
                ])
            end
        )
    end);
reflect({type, _, union, Ts}, At) ->
    shift_branch(fun(K) ->
        bind(
            sequence(
                lists:map(
                    fun
                        ({atom, _, Tag}) ->
                            bind(
                                reset(K({sem_atom, Tag})),
                                fun(E) ->
                                    return({
                                        ast_utils:make_at(Tag),
                                        E
                                    })
                                end
                            );
                        ({type, _, tuple, [{atom, _, Tag} | Types]}) ->
                            bind(
                                reflect_seq(Types, fun(Sems) ->
                                    K({sem_tuple, [{sem_atom, Tag} | Sems]})
                                end),
                                fun({Vars, E}) ->
                                    return({
                                        ast_utils:make_tuple([
                                            ast_utils:make_at(Tag)
                                            | Vars
                                        ]),
                                        E
                                    })
                                end
                            )
                    end,
                    Ts
                )
            ),
            fun(Clauses) -> return([ast_utils:make_case(At, Clauses)]) end
        )
    end).

-spec reflect_seq([tp()], fun(([sem()]) -> m([expr()], [expr()]))) -> m(sem(), [expr()]).
reflect_seq(Types, K) ->
    bind(new_vars(Types), fun(Vars) ->
        bind(
            reset(
                bind(
                    sequence(lists:zipwith(fun reflect/2, Types, Vars)),
                    K
                )
            ),
            fun(E) -> return({Vars, E}) end
        )
    end).

-spec reflect2(tp(), fun((sem()) -> m([expr()], [expr()]))) -> m({expr(), [expr()]}, [expr()]).
reflect2(T, K) ->
    bind(new_var(), fun(Var) ->
        bind(
            reset(bind(reflect(T, Var), K)),
            fun(V) -> return({Var, V}) end
        )
    end).

-spec call(tp(), atom(), [sem()]) -> m(sem(), [expr()]).
call({type, _, 'fun', [{type, _, product, Ts}, T2]}, Name, Sems) ->
    bind(
        reify_seq(Ts, Sems),
        fun(Vs) ->
            shift(fun(K) ->
                bind(
                    reflect2(T2, K),
                    fun({Var, Rest}) ->
                        return([
                            ast_utils:make_match(Var, ast_utils:make_call(Name, Vs))
                            | Rest
                        ])
                    end
                )
            end)
        end
    ).
