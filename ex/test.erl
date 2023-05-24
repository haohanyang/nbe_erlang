-module(test).
-compile(export_all).

-spec simple1(integer()) -> fun((integer()) -> integer()).
simple1(X) ->
    fun(Y) ->
        if
            false -> X;
            true -> Y
        end
    end.

simple1() -> ok.

-spec simple2(integer(), integer()) -> integer().
simple2(X, Y) ->
    if
        false -> X;
        true -> Y
    end.

-spec simple3(boolean()) -> boolean().
simple3(X) -> X.

-spec simple4(fun((integer()) -> integer())) ->
    fun((integer()) -> integer()).
simple4(X) -> X.

-spec simple5(boolean(), fun((integer()) -> integer())) -> integer().
simple5(B, F) ->
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
    end.

-spec simple5a(boolean(), fun((integer()) -> integer())) -> integer().
simple5a(B, F) ->
    F(
        if
            B -> 1;
            true -> 2
        end
    ).

-spec simple6(fun((integer()) -> integer()), integer()) -> integer().
simple6(F, X) ->
    Main =
        fun(F2, G, X2) ->
            G(F2(G(F2(G(F2(G(X2)))))))
        end,
    Main(F, fun(X3) -> X3 end, X).

-spec simple6a(integer()) -> integer().
simple6a(A) ->
    B = fun() -> A end,
    B().

-spec a:f(integer()) -> integer().

-spec simple7(integer()) -> integer().
simple7(X) -> a:f(X).

-spec a:g(integer()) -> boolean().

-spec simple8(integer()) -> boolean().
simple8(X) -> a:g(X).

-spec simple9({integer(), string()}) -> integer().
simple9(X) -> element(1, X).

-spec simple_add1(integer(), integer()) -> integer().
simple_add1(X, Y) -> 1 + X.

-spec simple_add2(integer(), integer()) -> integer().
simple_add2(X, Y) -> Y + X.

-spec simple_times1(integer(), integer()) -> integer().
simple_times1(X, Y) -> 1 * X.

-spec simple_times2(integer(), integer()) -> integer().
simple_times2(X, Y) -> Y * X.

-spec tuple({integer(), integer()}) -> {integer(), integer()}.
tuple(A) -> A.

-spec swap({integer(), integer()}) -> {integer(), integer()}.
swap({A, B}) -> {B, A}.

-spec element1(integer()) -> integer().
element1(A) -> element(A, {2, 3}).

-spec element2({integer(), integer()}, integer()) -> integer().
element2(A, X) -> element(X, A).

-spec element3({integer(), boolean()}) -> boolean().
element3(A) -> element(2, A).

-spec bool_tuple({boolean(), boolean()}) -> integer().
bool_tuple(A) -> 0.

-spec bool_tuple_2({boolean(), boolean()}) -> integer().
bool_tuple_2(V1) ->
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
    end.

-spec a:f() -> integer().

-spec bool_tuple2({boolean(), boolean()}) -> integer().
bool_tuple2(A) -> a:f().

-spec sum1(true | false) -> boolean().
sum1(X) -> X.

-spec sum2(a | {a}) -> integer().
sum2(X) ->
    case X of
        a -> 0;
        {a} -> 1;
        {a, A, B} -> A + B
    end.

-spec b:f() -> integer().
-spec b:g() -> integer().
-spec b:h() -> integer().

-spec multi_call() -> {integer(), integer(), integer()}.
multi_call() -> {b:f(), b:g(), b:h()}.

-spec io:format(string()) -> ok.

-spec print1(string()) -> ok.
print1(A) -> io:format(A).

-spec print2(string()) -> ok.
print2(A) -> (fun io:format/1)(A).

-spec rec1() -> integer().
rec1() -> fib(9).

fib(1) -> 1;
fib(2) -> 1;
fib(X) -> fib(X - 2) + fib(X - 1).

-spec rec2(integer()) -> integer().
rec2(A) -> pow(A, 4).

pow(X, 0) -> 1;
pow(X, N) -> X * pow(X, N - 1).

-spec sum_out(boolean()) -> true | false.
sum_out(A) -> A.

-spec sum_out2(boolean(), fun(() -> integer())) -> {ok, integer()} | error.
sum_out2(A, F) -> if A -> {ok, F()}; true -> error end.
