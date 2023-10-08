-module(never_stop).
-behaviour(gen_server).
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).

init(State) -> 
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(_Message, State) ->
    {noreply, State}.