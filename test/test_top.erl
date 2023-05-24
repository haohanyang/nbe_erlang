-module(test_top).

-include_lib("eunit/include/eunit.hrl").

top_test() ->
    {ok, In0} = epp:parse_file("ex/test.erl", []),
    In = erl_parse:map_anno(fun(_) -> 0 end, In0),
    {ok, Out0} = epp:parse_file("ex/test.out.erl", []),
    Out = erl_parse:map_anno(fun(_) -> 0 end, Out0),
    Result = nbe:norm_module(In),
    lists:zipwith(
        fun
            (R, R) ->
                ok;
            ({attribute, _, file, _}, {attribute, _, file, _}) ->
                ok;
            ({eof, _}, {eof, _}) ->
                ok;
            (R, O) ->
                Rs = catch forms:from_abstract(R),
                Os = catch forms:from_abstract(O),
                io:format("~n"),
                if
                    Rs == Os ->
                        io:format("expected:~n~p~n", [O]),
                        io:format("got:~n~p~n", [R]);
                    true ->
                        io:format("expected:~n~s~n", [Os]),
                        io:format("got:~n~s~n", [Rs])
                end,
                ?assert(false)
        end,
        Result,
        Out
    ).
