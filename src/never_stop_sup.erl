-module(never_stop_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(ChildSpecs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, ChildSpecs).

init([_ChildSpec | _] = ChildSpecs) ->
    SupFlags = #{strategy => one_for_all},
    {ok, {SupFlags, ChildSpecs}};

init([]) ->
    SupFlags = #{strategy => one_for_all},
    ChildSpecs = [
        #{
            id => cant_stop_wont_stop,
            start => {never_stop, start_link, [cant_stop_wont_stop]},
            restart => permanent
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.