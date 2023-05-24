-module(eval).
-export([run/2, run_get/2, eval/1]).
-export([ctx/0, ctx/2]).

% State over continuation monad
-type m(A) :: {state, fun((ctx()) -> rere:m({ctx(), A}, [expr()]))}.

return(X) -> lift(rere:return(X)).
bind(M = {state, _}, K) ->
    {state, fun(S) ->
        rere:bind(
            run_get(S, M),
            fun({S1, A}) ->
                run_get(S1, K(A))
            end
        )
    end}.
lift(F = {rere, _}) ->
    {state, fun(S) ->
        rere:bind(
            F,
            fun(V) -> rere:return({S, V}) end
        )
    end}.

sequence([]) ->
    return([]);
sequence([M | Ms]) ->
    bind(M, fun(N) -> bind(sequence(Ms), fun(Ns) -> return([N | Ns]) end) end).

-type expr() :: erl_parse:abstract_expr().
% Strings: Remember that in erlang semantics, a string is just a list of int. Not sure if we should try to ascribe different semantics to it.
% Bools: Likewise, a bool is just an atom.
% Functions: An alternative encoding could be { sem_fun, Ctx :: ctx(), Var :: string(), Body :: [abstract_expr()] }.
%            In that case we would need to distinguish between known and unknown functions, but we will do that already for other types, so why not?
-type sem() ::
    {sem_var, atom()}
    | {sem_int, integer()}
    | {sem_str, string()}
    | {sem_atom, atom()}
    | {sem_fun, fun(([sem()]) -> rere:m(sem(), [expr()]))}
    | {sem_tuple, [sem()]}
    | {sem_list, [sem()]}.

% This should really be a separate context and environment, but whatever
-record(ctx, {
    scope = #{} :: #{atom() => sem()},
    remote = #{} :: #{{atom(), atom(), integer()} => erl_parse:abstract_type()},
    local = #{} :: #{{atom(), integer()} => erl_parse:abstract_form()},
    current = nil :: {atom(), integer()}
}).
-type ctx() :: #ctx{}.

ctx() -> #ctx{}.
ctx(Local, Remote) -> #ctx{local = Local, remote = Remote}.

run(S, M) -> rere:bind(run_get(S, M), fun({_, O}) -> rere:return(O) end).
run_get(S = #ctx{}, {state, M}) -> M(S).
state(F) -> {state, fun(S) -> rere:return(F(S)) end}.
get_state(F) -> bind(state(fun(S) -> {S, S} end), F).

-spec eval(expr()) -> m(sem()).
%% eval on a single expression
eval({integer, _, I}) ->
    return({sem_int, I});
eval({string, _, S}) ->
    return({sem_str, S});
eval({atom, _, A}) ->
    return({sem_atom, A});
eval({nil, Anno}) ->
    List = extract_list({nil, Anno}),
    bind(eval_list(List), fun(Sems) -> return({sem_list, Sems}) end);
eval({cons, Anno, Ele, Rest}) ->
    List = extract_list({cons, Anno, Ele, Rest}),
    bind(eval_list(List), fun(Sems) -> return({sem_list, Sems}) end);
eval({tuple, _, Exprs}) ->
    bind(
        eval_list(Exprs),
        fun(Sems) -> return({sem_tuple, Sems}) end
    );
eval({var, _, V}) ->
    bind(
        lookup(V),
        fun({ok, Sem}) -> return(Sem) end
    );
eval({match, _, Pat, Expr}) ->
    bind(
        eval(Expr),
        fun(Sem) ->
            bind(
                match(Sem, Pat),
                fun(true) -> return(Sem) end
            )
        end
    );
eval({'if', _, Clauses}) ->
    bind(
        find_clause([], Clauses),
        fun(Clause) -> eval_clause(Clause) end
    );
eval({'case', _, Expr, Clauses}) ->
    bind(
        eval(Expr),
        fun(Sem) ->
            bind(
                find_clause([Sem], Clauses),
                fun(Clause) -> eval_clause(Clause) end
            )
        end
    );
eval({'fun', _, {clauses, Clauses}}) ->
    get_state(fun(S) ->
        return(
            {sem_fun, fun(Args) ->
                run(
                    S#ctx{scope = #{}},
                    bind(
                        find_clause(Args, Clauses),
                        fun(Clause) ->
                            bind(
                                state(fun(S2) ->
                                    {
                                        S2#ctx{
                                            scope = maps:merge(S#ctx.scope, S2#ctx.scope)
                                        },
                                        ok
                                    }
                                end),
                                fun(ok) -> eval_clause(Clause) end
                            )
                        end
                    )
                )
            end}
        )
    end);
eval({'fun', A, {function, Name, Arity}}) ->
    eval(wrap_call({atom, A, Name}, Arity));
eval({'fun', A, {function, Module, Name, {integer, _, Arity}}}) ->
    eval(wrap_call({remote, A, Module, Name}, Arity));
eval({call, _, {remote, _, {atom, _, Module}, {atom, _, Name}}, Args}) ->
    call_fun(Module, Name, Args);
eval({call, _, {atom, _, Name}, Args}) ->
    call_fun({}, Name, Args);
eval({call, _, F, Args}) ->
    bind(
        eval_list([F | Args]),
        fun([Fun | Sems]) -> call(Fun, Sems) end
    );
eval({op, _, O, L, R}) ->
    bind(
        eval_list([L, R]),
        fun([Ls, Rs]) -> eval_op(O, Ls, Rs) end
    ).

call({sem_fun, Fun}, Args) -> lift(Fun(Args)).

lookup(V) ->
    get_state(fun(S) ->
        return(maps:find(V, S#ctx.scope))
    end).

call_fun({}, element, Args = [_, _]) ->
    call_fun(erlang, element, Args);
call_fun(erlang, element, Args = [_, _]) ->
    bind(eval_list(Args), fun
        ([{sem_int, Int}, {sem_tuple, Elems}]) ->
            return(lists:nth(Int, Elems));
        (Sems) ->
            lift(
                rere:call(
                    ast_utils:parse_type("fun((integer(), {integer(), integer()}) -> integer())"),
                    {erlang, element},
                    Sems
                )
            )
    end);
call_fun({}, Name, Args) ->
    bind(eval_list(Args), fun(Sems) ->
        get_state(fun(S) ->
            Fun = maps:get({Name, length(Args)}, S#ctx.local),
            lift(
                run(
                    S#ctx{scope = #{}},
                    bind(eval(Fun), fun(F) -> call(F, Sems) end)
                )
            )
        end)
    end);
call_fun(Mod, Name, Args) ->
    bind(eval_list(Args), fun(Sems) ->
        get_state(fun(S) ->
            Type = maps:get({Mod, Name, length(Args)}, S#ctx.remote),
            lift(rere:call(Type, {Mod, Name}, Sems))
        end)
    end).

wrap_call(Name, Arity) ->
    Vars = [{var, 0, N} || N <- lists:seq(1, Arity)],
    {'fun', 0, {clauses, [{clause, 0, Vars, [], [{call, 0, Name, Vars}]}]}}.

-define(i(T), {sem_int, T}).

eval_op_('+', ?i(L), ?i(R)) -> ?i(L + R);
eval_op_('+', ?i(0), R) -> R;
eval_op_('+', L, ?i(0)) -> L;
eval_op_('-', ?i(L), ?i(R)) -> ?i(L - R);
% TODO negative
eval_op_('-', ?i(0), R) -> R;
eval_op_('-', L, ?i(0)) -> L;
eval_op_('*', ?i(L), ?i(R)) -> ?i(L * R);
eval_op_('*', ?i(0), _) -> ?i(0);
eval_op_('*', _, ?i(0)) -> ?i(0);
eval_op_('*', ?i(1), R) -> R;
eval_op_('*', L, ?i(1)) -> L;
eval_op_(_, _, _) -> err.

eval_op(O, L, R) ->
    case eval_op_(O, L, R) of
        err ->
            lift(
                rere:call(
                    ast_utils:parse_type("fun((integer(), integer()) -> integer())"),
                    {O},
                    [L, R]
                )
            );
        V ->
            return(V)
    end.

-spec extract_list(expr()) -> list(expr()).
extract_list({nil, _}) -> [];
extract_list({cons, _, Ele, Rest}) -> [Ele | extract_list(Rest)].

-spec find_clause([sem()], [erl_parse:abstract_clause()]) -> m([expr()]).
find_clause(Sems, [{clause, _, Pats, Guards, Body} | Clauses]) ->
    bind(
        all(
            [match(V, P) || {V, P} <- lists:zip(Sems, Pats)] ++
                [eval_cond(Cond) || Guard <- Guards, Cond <- Guard]
        ),
        fun
            (true) -> return(Body);
            (false) -> find_clause(Sems, Clauses)
        end
    ).

-spec match(sem(), expr()) -> m(boolean()).
match(Sem, Pat) ->
    get_state(fun(S) ->
        bind(match_(Sem, Pat), fun
            (true) -> return(true);
            (false) -> state(fun(_) -> {S, false} end)
        end)
    end).

match_(Sem, {var, _, V}) ->
    bind(lookup(V), fun
        ({ok, Val2}) -> return(Sem == Val2);
        (error) -> state(fun(S) -> {S#ctx{scope = maps:put(V, Sem, S#ctx.scope)}, true} end)
    end);
match_({sem_tuple, Ss}, {tuple, _, Ps}) when length(Ss) == length(Ps) ->
    all([match_(S, P) || {S, P} <- lists:zip(Ss, Ps)]);
match_({sem_atom, A}, {atom, _, A}) ->
    return(true);
match_({sem_int, A}, {integer, _, A}) ->
    return(true);
match_({sem_list, []}, {nil, _}) ->
    return(true);
match_({sem_list, [S | Ss]}, {cons, _, P, Ps}) ->
    and_(match_(S, P), match_({sem_list, Ss}, Ps));
match_(_, _) ->
    return(false).

and_(A, B) ->
    bind(
        A,
        fun
            (true) -> B;
            (false) -> return(false)
        end
    ).

all([]) ->
    return(true);
all([M | Rest]) ->
    and_(M, all(Rest)).

-spec eval_cond(expr()) -> m(boolean()).
eval_cond(Expr) ->
    bind(
        eval(Expr),
        fun
            ({sem_atom, true}) -> return(true);
            ({sem_atom, false}) -> return(false)
        end
    ).

-spec eval_list([expr()]) -> m([sem()]).
eval_list(Exprs) -> sequence([eval(X) || X <- Exprs]).

-spec eval_clause([expr()]) -> m(sem()).
eval_clause(Exprs) -> bind(eval_list(Exprs), fun(A) -> return(lists:last(A)) end).
