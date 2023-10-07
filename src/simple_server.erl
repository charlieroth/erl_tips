-module(simple_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_continue/2, handle_cast/2]).

init(Value) ->
    {ok, Value, {continue, post_init}}.

handle_call(do_stuff, _From, State) ->
    {reply, State, State, {continue, post_call}}.

handle_continue(post_init, State) ->
    io:format("post_init handle_continue~n"),
    {noreply, State};

handle_continue(post_call, State) ->
    io:format("post_call handle_continue~n"),
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.