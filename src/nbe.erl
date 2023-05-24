-module(nbe).

-export([norm_module/1, norm/2, norm/3]).
-export([main/1]).

-type tp() :: erl_parse:abstract_type().
-type expr() :: eval:expr().

-spec norm(tp(), expr()) -> expr().
norm(Type, Expr) -> norm(Type, Expr, eval:ctx()).

-spec norm(tp(), expr(), eval:ctx()) -> expr().
norm(Type, Expr, Context) ->
    [A] = rere:run(
        rere:bind(
            eval:run(Context, eval:eval(Expr)),
            fun(Sem) -> rere:reify(Type, Sem) end
        )
    ),
    A.

-record(collect, {
    remote_type = #{} :: #{{atom(), atom(), integer()} => erl_parse:abstract_type()},
    local_type = #{} :: #{{atom(), integer()} => erl_parse:abstract_type()},
    local_body = #{} :: #{{atom(), integer()} => erl_parse:abstract_expr()}
}).

-spec norm_module([erl_parse:abstract_form()]) -> [erl_parse:abstract_form()].
norm_module(Forms) ->
    C = collect_module(Forms, #collect{}),
    Ctx = eval:ctx(C#collect.local_body, C#collect.remote_type),
    lists:filtermap(
        fun
            ({function, _, Name, Arity, _} = F) ->
                case maps:find({Name, Arity}, C#collect.local_type) of
                    {ok, Ty} ->
                        Norm = nbe:norm(Ty, ast_utils:function2fun(F), Ctx),
                        {true, ast_utils:fun2function(Name, Arity, Norm)};
                    error ->
                        false
                end;
            (_) ->
                true
        end,
        Forms
    ).

collect_module([], C) ->
    C;
collect_module([{attribute, _, spec, {{Name, Arity}, [Ft]}} | Forms], C) ->
    V = maps:put({Name, Arity}, Ft, C#collect.local_type),
    collect_module(Forms, C#collect{local_type = V});
collect_module([{attribute, _, spec, {{Mod, Name, Arity}, [Ft]}} | Forms], C) ->
    V = maps:put({Mod, Name, Arity}, Ft, C#collect.remote_type),
    collect_module(Forms, C#collect{remote_type = V});
collect_module([{function, _, Name, Arity, _Clauses} = F | Forms], C) ->
    V = maps:put({Name, Arity}, ast_utils:function2fun(F), C#collect.local_body),
    collect_module(Forms, C#collect{local_body = V});
collect_module([_ | Forms], C) ->
    collect_module(Forms, C).

main([File]) ->
    {ok, In} = epp:parse_file(File, []),
    Result = nbe:norm_module(In),
    io:format("~s~n", [forms:from_abstract(Result)]);
main(_) ->
    io:format("One argument required").
