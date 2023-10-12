-module(production_only_server).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2]).

start_link([]) ->
    gen_server:start_link(?MODULE, [], []).

init(State) ->
    {ok, AppEnv} = application:get_env(erl_tips, ?MODULE),
    case AppEnv of
        {start_process, true} ->
            {ok, State};
        _ ->
            ignore
    end.

handle_call(_Message, _From, State) ->
    {reply, State, State}.

handle_cast(_Message, State) ->
    {noreply, State}.
