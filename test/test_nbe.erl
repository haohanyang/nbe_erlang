-module(test_nbe).

-import(nbe, [norm/3]).
-import(eval, [ctx/0, ctx/2]).

-include_lib("eunit/include/eunit.hrl").

-define(f(T),
    (fun() ->
        [A] = ast_utils:to_abstract(??T ++ "."),
        A
    end)()
).

-define(test(T, FT, F1, F2),
    Abs1 = ?f(F1),
    Abs2 = ?f(F2),
    Norm = norm(T, Abs1, FT),
    ?assertEqual(forms:from_abstract(Abs2), forms:from_abstract(Norm))
).

t(ty_int) ->
    {type, 0, integer, []};
t(ty_bool) ->
    {type, 0, boolean, []};
t(ty_str) ->
    {type, 0, string, []};
t(ty_atom) ->
    {type, 0, atom, []};
t(ty_unit) ->
    {atom, 0, ok};
t({ty_sum, Ss}) ->
    {type, 0, union,
        lists:map(
            fun
                (A) when is_atom(A) -> {atom, 0, A};
                ({A, Ts}) -> {type, 0, tuple, [{atom, 0, A} | lists:map(fun t/1, Ts)]}
            end,
            Ss
        )};
t({ty_tuple, Ts}) ->
    {type, 0, tuple, lists:map(fun t/1, Ts)};
t({ty_fun, Ts, T2}) ->
    {type, 0, 'fun', [{type, 0, product, lists:map(fun t/1, Ts)}, t(T2)]}.

simple1_test() ->
    ?test(
        t({ty_fun, [ty_int], {ty_fun, [ty_int], ty_int}}),
        ctx(),
        fun(X) ->
            fun(Y) ->
                if
                    false -> X;
                    true -> Y
                end
            end
        end,
        fun(V1) -> fun(V2) -> V2 end end
    ).

simple2_test() ->
    ?test(
        t({ty_fun, [ty_int, ty_int], ty_int}),
        ctx(),
        fun(X, Y) ->
            if
                false -> X;
                true -> Y
            end
        end,
        fun(V1, V2) -> V2 end
    ).

simple3_test() ->
    ?test(
        t({ty_fun, [ty_bool], ty_bool}),
        ctx(),
        fun(X) -> X end,
        fun(V1) ->
            if
                V1 -> true;
                true -> false
            end
        end
    ).

simple4_test() ->
    ?test(
        t({ty_fun, [{ty_fun, [ty_int], ty_int}], {ty_fun, [ty_int], ty_int}}),
        ctx(),
        fun(B) -> B end,
        fun(V1) ->
            fun(V2) ->
                V3 = V1(V2),
                V3
            end
        end
    ).

simple5_test() ->
    ?test(
        t({ty_fun, [ty_bool, {ty_fun, [ty_int], ty_int}], ty_int}),
        ctx(),
        fun(B, F) ->
            if
                B ->
                    F(
                        if
                            B -> 2;
                            true -> 8
                        end
                    );
                true ->
                    F(
                        if
                            B -> 2;
                            true -> 8
                        end
                    )
            end
        end,
        fun(V1, V2) ->
            if
                V1 ->
                    V3 = V2(2),
                    V3;
                true ->
                    V3 = V2(8),
                    V3
            end
        end
    ).

simple5a_test() ->
    ?test(
        t({ty_fun, [ty_bool, {ty_fun, [ty_int], ty_int}], ty_int}),
        ctx(),
        fun(B, F) ->
            F(
                if
                    B -> 1;
                    true -> 2
                end
            )
        end,
        fun(V1, V2) ->
            if
                V1 ->
                    V3 = V2(1),
                    V3;
                true ->
                    V3 = V2(2),
                    V3
            end
        end
    ).

simple6_test() ->
    ?test(
        t({ty_fun, [{ty_fun, [ty_int], ty_int}, ty_int], ty_int}),
        ctx(),
        fun(F, X) ->
            Main =
                fun(F2, G, X2) ->
                    G(F2(G(F2(G(F2(G(X2)))))))
                end,
            Main(F, fun(X3) -> X3 end, X)
        end,
        fun(V1, V2) ->
            V3 = V1(V2),
            V4 = V1(V3),
            V5 = V1(V4),
            V5
        end
    ).

simple6a_test() ->
    ?test(
        t({ty_fun, [ty_int], ty_int}),
        ctx(),
        fun(A) ->
            B = fun() -> A end,
            B()
        end,
        fun(V1) -> V1 end
    ).

simple7_test() ->
    ?test(
        t({ty_fun, [ty_int], ty_int}),
        ctx(#{}, #{
            {a, f, 1} => t({ty_fun, [ty_int], ty_int})
        }),
        fun(X) ->
            a:f(X)
        end,
        fun(V1) ->
            V2 = a:f(V1),
            V2
        end
    ).

simple8_test() ->
    ?test(
        t({ty_fun, [ty_int], ty_bool}),
        ctx(#{}, #{
            {a, f, 1} => t({ty_fun, [ty_int], ty_bool})
        }),
        fun(X) ->
            a:f(X)
        end,
        fun(V1) ->
            V2 = a:f(V1),
            if
                V2 -> true;
                true -> false
            end
        end
    ).

simple9_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_int, ty_str]}], ty_int}),
        ctx(),
        fun(X) ->
            element(1, X)
        end,
        fun(V1) ->
            {V2, V3} = V1,
            V2
        end
    ).

simple_add1_test() ->
    ?test(
        t({ty_fun, [ty_int, ty_int], ty_int}),
        ctx(),
        fun(X, Y) -> 1 + X end,
        fun(V1, V2) ->
            V3 = 1 + V1,
            V3
        end
    ).

simple_add2_test() ->
    ?test(
        t({ty_fun, [ty_int, ty_int], ty_int}),
        ctx(),
        fun(X, Y) -> Y + X end,
        fun(V1, V2) ->
            V3 = V2 + V1,
            V3
        end
    ).

simple_times1_test() ->
    ?test(
        t({ty_fun, [ty_int, ty_int], ty_int}),
        ctx(),
        fun(X, Y) -> 1 * X end,
        fun(V1, V2) -> V1 end
    ).

simple_times2_test() ->
    ?test(
        t({ty_fun, [ty_int, ty_int], ty_int}),
        ctx(),
        fun(X, Y) -> Y * X end,
        fun(V1, V2) ->
            V3 = V2 * V1,
            V3
        end
    ).

tuple_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_int, ty_int]}], {ty_tuple, [ty_int, ty_int]}}),
        ctx(),
        fun(A) -> A end,
        fun(V1) ->
            {V2, V3} = V1,
            {V2, V3}
        end
    ).

swap_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_int, ty_int]}], {ty_tuple, [ty_int, ty_int]}}),
        ctx(),
        fun({A, B}) -> {B, A} end,
        fun(V1) ->
            {V2, V3} = V1,
            {V3, V2}
        end
    ).

element1_test() ->
    ?test(
        t({ty_fun, [ty_int], ty_int}),
        ctx(),
        fun(A) -> element(A, {2, 3}) end,
        fun(V1) ->
            V2 = erlang:element(V1, {2, 3}),
            V2
        end
    ).

element2_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_int, ty_int]}, ty_int], ty_int}),
        ctx(),
        fun(A, X) -> element(X, A) end,
        fun(V1, V2) ->
            {V3, V4} = V1,
            V5 = erlang:element(V2, {V3, V4}),
            V5
        end
    ).

element3_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_int, ty_bool]}], ty_bool}),
        ctx(),
        fun(A) -> element(2, A) end,
        fun(V1) ->
            {V2, V3} = V1,
            if
                V3 -> true;
                true -> false
            end
        end
    ).

bool_tuple_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_bool, ty_bool]}], ty_int}),
        ctx(),
        fun(A) -> 0 end,
        fun(V1) ->
            {V2, V3} = V1,
            0
        end
    ).

bool_tuple_2_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_bool, ty_bool]}], ty_int}),
        ctx(),
        fun(V1) ->
            {V2, V3} = V1,
            if
                V2 ->
                    if
                        V3 -> 0;
                        true -> 0
                    end;
                true ->
                    if
                        V3 -> 0;
                        true -> 0
                    end
            end
        end,
        fun(V1) ->
            {V2, V3} = V1,
            0
        end
    ).

bool_tuple2_test() ->
    ?test(
        t({ty_fun, [{ty_tuple, [ty_bool, ty_bool]}], ty_int}),
        ctx(#{}, #{
            {a, f, 0} => t({ty_fun, [], ty_int})
        }),
        fun(A) -> a:f() end,
        fun(V1) ->
            {V2, V3} = V1,
            V4 = a:f(),
            V4
        end
    ).

sum1_test() ->
    ?test(
        t({ty_fun, [{ty_sum, [true, false]}], ty_bool}),
        ctx(),
        fun(X) -> X end,
        fun(V1) ->
            case V1 of
                true -> true;
                false -> false
            end
        end
    ).

sum2_test() ->
    ?test(
        t({ty_fun, [{ty_sum, [a, {a, []}]}], ty_int}),
        ctx(),
        fun(X) ->
            case X of
                a -> 0;
                {a} -> 1;
                {a, A, B} -> A + B
            end
        end,
        fun(V1) ->
            case V1 of
                a -> 0;
                {a} -> 1
            end
        end
    ).

multi_call_test() ->
    ?test(
        t({ty_fun, [], {ty_tuple, [ty_int, ty_int, ty_int]}}),
        ctx(#{}, #{
            {a, f, 0} => t({ty_fun, [], ty_int}),
            {a, g, 0} => t({ty_fun, [], ty_int}),
            {a, h, 0} => t({ty_fun, [], ty_int})
        }),
        fun() ->
            {a:f(), a:g(), a:h()}
        end,
        fun() ->
            V1 = a:f(),
            V2 = a:g(),
            V3 = a:h(),
            {V1, V2, V3}
        end
    ).

print1_test() ->
    ?test(
        t({ty_fun, [ty_str], ty_unit}),
        ctx(#{}, #{
            {io, format, 1} => t({ty_fun, [ty_str], ty_unit})
        }),
        fun(A) ->
            io:format(A)
        end,
        fun(V1) ->
            V2 = io:format(V1),
            ok
        end
    ).

print2_test() ->
    ?test(
        t({ty_fun, [ty_str], ty_unit}),
        ctx(#{}, #{
            {io, format, 1} => t({ty_fun, [ty_str], ty_unit})
        }),
        fun(A) ->
            (fun io:format/1)(A)
        end,
        fun(V1) ->
            V2 = io:format(V1),
            ok
        end
    ).

rec1_test() ->
    ?test(
        t({ty_fun, [], ty_int}),
        ctx(
            #{
                {fib, 1} => ?f(fun
                    (1) -> 1;
                    (2) -> 1;
                    (X) -> fib(X - 2) + fib(X - 1)
                end)
            },
            #{}
        ),
        fun() -> fib(9) end,
        fun() -> 34 end
    ).

rec2_test() ->
    ?test(
        t({ty_fun, [ty_int], ty_int}),
        ctx(
            #{
                {pow, 2} => ?f(fun
                    (X, 0) -> 1;
                    (X, N) -> X * pow(X, N - 1)
                end)
            },
            #{}
        ),
        fun(A) ->
            pow(A, 4)
        end,
        fun(V1) ->
            V2 = V1 * V1,
            V3 = V1 * V2,
            V4 = V1 * V3,
            V4
        end
    ).
