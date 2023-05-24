-module(aux_server).

-import(erl_syntax_lib, [new_variable_name/1]).

-compile(export_all).

-type state() :: integer().

-behaviour(gen_server).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, 1, []).

init(InitState) ->
    {ok, InitState}.

new_var() ->
    gen_server:call(?MODULE, new_var).

stop() ->
    gen_server:call(?MODULE, stop).

handle_call(new_var, _From, Counter) ->
    Var = list_to_atom("V" ++ integer_to_list(Counter)),
    {reply, Var, Counter + 1};
handle_call(reset, _From, _Counter) ->
    {reply, ok, 1};
handle_call(stop, _From, _Counter) ->
    {stop, normal, ok, 1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
