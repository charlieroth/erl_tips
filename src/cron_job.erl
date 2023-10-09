-module(cron_job).
-behaviour(gen_server).
-export([
    start_link/1, 
    init/1, 
    handle_continue/2, 
    handle_info/2,
    handle_call/3,
    handle_cast/2
]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(State) ->
    {ok, State, {continue, schedule_timer}}.

handle_continue(schedule_timer, {Timer, _JobFun} = State) ->
    {ok, _} = timer:send_after(Timer, self(), run),
    {noreply, State}.

handle_info(run, {_Timer, JobFun} = State) ->
    JobFun(),
    {noreply, State, {continue, schedule_timer}}.

handle_call(_Message, _From, State) ->
    {reply, State, State}.

handle_cast(_Message, State) ->
    {noreply, State}.