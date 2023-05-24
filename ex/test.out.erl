-module(test).
-compile(export_all).

-spec simple1(integer()) -> fun((integer()) -> integer()).
simple1(V1) -> fun(V2) -> V2 end.

-spec simple2(integer(), integer()) -> integer().
simple2(V1, V2) -> V2.

-spec simple3(boolean()) -> boolean().
simple3(V1) ->
    if
        V1 -> true;
        true -> false
    end.

-spec simple4(fun((integer()) -> integer())) -> fun((integer()) -> integer()).
simple4(V1) ->
    fun(V2) ->
        V3 = V1(V2),
        V3
    end.

-spec simple5(boolean(), fun((integer()) -> integer())) -> integer().
simple5(V1, V2) ->
    if
        V1 ->
            V3 = V2(2),
            V3;
        true ->
            V3 = V2(8),
            V3
    end.

-spec simple5a(boolean(), fun((integer()) -> integer())) -> integer().
simple5a(V1, V2) ->
    if
        V1 ->
            V3 = V2(1),
            V3;
        true ->
            V3 = V2(2),
            V3
    end.

-spec simple6(fun((integer()) -> integer()), integer()) -> integer().
simple6(V1, V2) ->
    V3 = V1(V2),
    V4 = V1(V3),
    V5 = V1(V4),
    V5.

-spec simple6a(integer()) -> integer().
simple6a(V1) -> V1.

-spec a:f(integer()) -> integer().

-spec simple7(integer()) -> integer().
simple7(V1) ->
    V2 = a:f(V1),
    V2.

-spec a:g(integer()) -> boolean().

-spec simple8(integer()) -> boolean().
simple8(V1) ->
    V2 = a:g(V1),
    if
        V2 -> true;
        true -> false
    end.

-spec simple9({integer(), string()}) -> integer().
simple9(V1) ->
    {V2, V3} = V1,
    V2.

-spec simple_add1(integer(), integer()) -> integer().
simple_add1(V1, V2) ->
    V3 = 1 + V1,
    V3.

-spec simple_add2(integer(), integer()) -> integer().
simple_add2(V1, V2) ->
    V3 = V2 + V1,
    V3.

-spec simple_times1(integer(), integer()) -> integer().
simple_times1(V1, V2) ->
    V1.

-spec simple_times2(integer(), integer()) -> integer().
simple_times2(V1, V2) ->
    V3 = V2 * V1,
    V3.

-spec tuple({integer(), integer()}) -> {integer(), integer()}.
tuple(V1) ->
    {V2, V3} = V1,
    {V2, V3}.

-spec swap({integer(), integer()}) -> {integer(), integer()}.
swap(V1) ->
    {V2, V3} = V1,
    {V3, V2}.

-spec element1(integer()) -> integer().
element1(V1) ->
    V2 = erlang:element(V1, {2, 3}),
    V2.

-spec element2({integer(), integer()}, integer()) -> integer().
element2(V1, V2) ->
    {V3, V4} = V1,
    V5 = erlang:element(V2, {V3, V4}),
    V5.

-spec element3({integer(), boolean()}) -> boolean().
element3(V1) ->
    {V2, V3} = V1,
    if
        V3 -> true;
        true -> false
    end.

-spec bool_tuple({boolean(), boolean()}) -> integer().
bool_tuple(V1) ->
    {V2, V3} = V1,
    0.

-spec bool_tuple_2({boolean(), boolean()}) -> integer().
bool_tuple_2(V1) ->
    {V2, V3} = V1,
    0.

-spec a:f() -> integer().

-spec bool_tuple2({boolean(), boolean()}) -> integer().
bool_tuple2(V1) ->
    {V2, V3} = V1,
    V4 = a:f(),
    V4.

-spec sum1(true | false) -> boolean().
sum1(V1) ->
    case V1 of
        true -> true;
        false -> false
    end.

-spec sum2(a | {a}) -> integer().
sum2(V1) ->
    case V1 of
        a -> 0;
        {a} -> 1
    end.

-spec b:f() -> integer().
-spec b:g() -> integer().
-spec b:h() -> integer().

-spec multi_call() -> {integer(), integer(), integer()}.
multi_call() ->
    V1 = b:f(),
    V2 = b:g(),
    V3 = b:h(),
    {V1, V2, V3}.

-spec io:format(string()) -> ok.

-spec print1(string()) -> ok.
print1(V1) ->
    V2 = io:format(V1),
    ok.

-spec print2(string()) -> ok.
print2(V1) ->
    V2 = io:format(V1),
    ok.

-spec rec1() -> integer().
rec1() -> 34.

-spec rec2(integer()) -> integer().
rec2(V1) ->
    V2 = V1 * V1,
    V3 = V1 * V2,
    V4 = V1 * V3,
    V4.

-spec sum_out(boolean()) -> true | false.
sum_out(V1) ->
    if V1 -> true;
       true -> false
    end.

-spec sum_out2(boolean(), fun(() -> integer())) -> {ok, integer()} | error.
sum_out2(V1, V2) ->
    if V1 ->
           V3 = V2(),
           {ok, V3};
       true -> error
    end.
