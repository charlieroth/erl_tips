-module(temp_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(ChildSpecs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, ChildSpecs).

init([_ChildSpec | _] = ChildSpecs) ->
    {ok, {#{strategy => one_for_all}, ChildSpecs}};

init([]) ->
    SupFlags = #{strategy => one_for_all},
    ChildSpecs = [
        #{
            id => can_stop_will_stop,
            start => {temp_server, start_link, [can_stop_will_stop]},
            restart => transient
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.