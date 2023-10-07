%%%-------------------------------------------------------------------
%% @doc erl_tips public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_tips_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_tips_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
