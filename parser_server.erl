-module(parser_server).
-behaviour(gen_server).
-export([parse/1, start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%  API  %%%%%%%%%%%%%%%%%%
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

parse(Files) -> gen_server:call(?MODULE, {parse, Files}).


%%%%%%% gen_server callback %%%%%%%%
init([]) -> {ok, []}.

handle_call({parse, Files}, From, State) ->
    spawn(fun() -> ctparser:parse(Files, From) end),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> 
    io:format("In handle_cast step! Msg is ~p, State is ~p ~n", [_Msg, State]),
    {noreply, State}.
handle_info(_Info, State) -> 
    io:format("In handle_info step! Info is ~p, State is ~p ~n", [_Info, State]),
    {noreply, State}.
terminate(_Reason, _State) -> 
    io:format("In terminate step! Reason is ~p, State is ~p ~n", [_Reason, _State]),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.