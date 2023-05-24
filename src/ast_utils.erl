-module(ast_utils).

-type form() :: erl_parse:abstract_form().
-type forms() :: list(form()).
-type expr() :: erl_parse:abstract_expr().
-type exprs() :: list(expr()).

-compile(export_all).

-spec get_all_fun_forms(file:name()) -> {ok, forms()} | {error, atom()}.
get_all_fun_forms(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            {ok, lists:filter(fun(Form) -> erl_syntax:type(Form) == function end, Forms)};
        Error ->
            Error
    end.

-spec get_fun_form(file:name(), atom()) -> {ok, forms()} | {error, atom()}.
get_fun_form(File, FunName) ->
    case get_all_fun_forms(File) of
        {ok, Forms} ->
            {ok, lists:filter(fun(Form) -> erl_syntax:function_name(Form) == FunName end, Forms)};
        Error ->
            Error
    end.

-spec function2fun(form()) -> expr().
function2fun({function, ANNO, _Name, _Arity, Clauses}) ->
    {'fun', ANNO, {clauses, Clauses}}.

-spec fun2function(atom(), integer(), expr()) -> form().
fun2function(Name, Arity, {'fun', ANNO, {clauses, Clauses}}) ->
    {function, ANNO, Name, Arity, Clauses}.

-spec parse_type(string()) -> erl_parse:abstract_type().
parse_type(S) ->
    {ok, Tokens, _EndLocation} = erl_scan:string("-type a() :: " ++ S ++ "."),
    TokensWithoutAnno = erl_parse:map_anno(fun(_) -> 0 end, Tokens),
    {ok, {attribute, _, type, {a, T, []}}} = erl_parse:parse_form(TokensWithoutAnno),
    T.

%% Convert the Erlang source code string to abstract format with annotation removed.
%% Useful to compare two ASTs regardless of line locations.
-spec to_abstract(string()) -> form().
to_abstract(String) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(String),
    TokensWithoutAnno = erl_parse:map_anno(fun(_) -> 0 end, Tokens),
    case erl_parse:parse_form(TokensWithoutAnno) of
        {ok, V} -> ok;
        {error, _} -> {ok, V} = erl_parse:parse_exprs(TokensWithoutAnno)
    end,
    V.

-spec make_int(integer()) -> expr().
make_int(Int) ->
    {integer, 0, Int}.

-spec make_tuple(exprs()) -> expr().
make_tuple(Exprs) ->
    {tuple, 0, Exprs}.

-spec make_str(string()) -> expr().
make_str(Str) ->
    {string, 0, Str}.

-spec make_at(atom()) -> expr().
make_at(At) ->
    {atom, 0, At}.

-spec make_var(atom()) -> expr().
make_var(Var) ->
    {var, 0, Var}.

-spec make_match(expr(), expr()) -> expr().
make_match(Pattern, Body) ->
    {match, 0, Pattern, Body}.

-spec make_call(atom() | {atom()} | {atom(), atom()}, exprs()) -> expr().
make_call({Module, Name}, Args) ->
    {call, 0, {remote, 0, make_at(Module), make_at(Name)}, Args};
make_call({Op}, [L, R]) ->
    {op, 0, Op, L, R};
make_call(Name = {var, _, _}, Args) ->
    {call, 0, Name, Args}.

-spec make_fun(exprs(), exprs()) -> expr().
make_fun(Paras, Exprs) ->
    Clause = {clause, 0, Paras, [], Exprs},
    {'fun', 0, {clauses, [Clause]}}.

%% Right now only if-else
-spec make_if_else(expr(), expr(), expr()) -> expr().
make_if_else(Cond, Exp1, Exp2) ->
    Clause1 = {clause, 0, [], [[Cond]], Exp1},
    Clause2 = {clause, 0, [], [[make_at(true)]], Exp2},
    {'if', 0, [Clause1, Clause2]}.

-spec make_case(expr(), [{expr(), expr()}]) -> expr().
make_case(E, Cs) ->
    {'case', 0, E, [{'clause', 0, [A], [], B} || {A, B} <- Cs]}.
