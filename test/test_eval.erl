-module(test_eval).

-include_lib("eunit/include/eunit.hrl").

-define(test_res(Expr, ExpectedRes),
    [Abs] = ast_utils:to_abstract(??Expr ++ "."),
    Res = rere:run(eval:run(eval:ctx(), eval:eval(Abs))),
    ?assertEqual(ExpectedRes, Res)
).

-define(test_ctx(Expr, Keys, Vals),
    [Abs] = ast_utils:to_abstract(??Expr ++ "."),
    {Ctx, _Res} = rere:run(eval:run_get(eval:ctx(), eval:eval(Abs))),
    {ctx, Scope, _, _, _} = Ctx,
    lists:zipwith(fun(K, V) -> ?assertEqual(maps:get(K, Scope), V) end, Keys, Vals)
).

simple_fun1a_test() ->
    ?test_res(
        (fun(X) ->
            R = 1,
            S = 1,
            R
        end)(
            2
        ),
        {sem_int, 1}
    ).

simple_fun1b_test() ->
    ?test_res(
        (fun(X) ->
            R = 1,
            S = 1,
            X
        end)(
            2
        ),
        {sem_int, 2}
    ).

simple_fun2_test() ->
    ?test_res(
        (fun(X, Y) ->
            X
        end)(
            2, 3
        ),
        {sem_int, 2}
    ).

simple_if_test() ->
    ?test_res(
        (fun(X) ->
            R = true,
            if
                R, true -> X;
                true -> 1
            end
        end)(
            3
        ),
        {sem_int, 3}
    ).

multi_if_test() ->
    ?test_res(
        (fun(X) ->
            R = false,
            if
                false, false, false -> 0;
                R -> 1;
                true -> X
            end
        end)(
            3
        ),
        {sem_int, 3}
    ).

simple_call_test() ->
    ?test_res(
        (fun(X) ->
            MyFun = fun(M) -> M end,
            MyFun(X)
        end)(
            1
        ),
        {sem_int, 1}
    ).

multi_arg_call_test() ->
    ?test_res(
        (fun() ->
            (fun(M, N) -> M end)(3, 2)
        end)(),
        {sem_int, 3}
    ).

simple_tuple_test() ->
    ?test_res(
        {1, 2, 3},
        {sem_tuple, [{sem_int, 1}, {sem_int, 2}, {sem_int, 3}]}
    ).

simple_add_test() ->
    ?test_res(
        1 + 2,
        {sem_int, 3}
    ).

simple_list1_test() ->
    ?test_res(
        [1, 2, 3], {sem_list, [{sem_int, 1}, {sem_int, 2}, {sem_int, 3}]}
    ).

simple_list2_test() ->
    ?test_res(
        [1], {sem_list, [{sem_int, 1}]}
    ).

simple_list_pattern_matching1_test() ->
    ?test_ctx([A | B] = [1, 2, 3], ['A', 'B'], [
        {sem_int, 1}, {sem_list, [{sem_int, 2}, {sem_int, 3}]}
    ]).

simple_tuple_pattern_matching1_test() ->
    ?test_ctx({A, B} = {1, 2}, ['A', 'B'], [
        {sem_int, 1}, {sem_int, 2}
    ]).
