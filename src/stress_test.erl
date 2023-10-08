-module(stress_test).
-export([post/3, get/2]).

post(Url, Payload, TotalReqs) ->
    {ok, _} = start_deps(),
    Req = {Url, [], "application/json", Payload},
    AvgTime = do_req(post, Req, TotalReqs),
    io:format("Avg response time from ~s is ~pms~n", [Url, AvgTime]).

get(Url, TotalReqs) ->
    {ok, _} = start_deps(),
    Req = {Url, []},
    AvgTime = do_req(get, Req, TotalReqs),
    io:format("Avg response time from ~s is ~pms~n", [Url, AvgTime]).

do_req(Method, Req, TotalReqs) ->
    Reqs = [{Method, Req} || _ <- lists:seq(1, TotalReqs)],
    Results = pmap(fun({Method1, Req1}) ->
        StartTime = erlang:system_time(millisecond),
        {ok, _} = httpc:request(Method1, Req1, [], []),
        EndTime = erlang:system_time(millisecond),
        {ok, EndTime - StartTime}
    end, Reqs, 5000),
    calculate_avg_time(Results, TotalReqs, 0).

calculate_avg_time([], TotalReqs, TotalTime) -> 
    TotalTime / TotalReqs;

calculate_avg_time([Result | Rest], TotalReqs, TotalTime) ->
    {ok, ReqTime} = Result,
    calculate_avg_time(Rest, TotalReqs, TotalTime + ReqTime).

start_deps() ->
    application:ensure_all_started([inets, ssl]).

pmap(F, Es, Timeout) ->
    Parent = self(),
    Running = [exec(Parent, F, E) || E <- Es],
    collect(Running, Timeout).

exec(Parent, F, E) ->
    spawn_monitor(fun() -> Parent ! {self(), F(E)} end).

collect([], _Timeout) ->
    [];
collect([{Pid, MRef} | Next], Timeout) ->
    receive
        {Pid, Res} ->
            erlang:demonitor(MRef, [flush]),
            [Res | collect(Next, Timeout)];
        {'DOWN', MRef, process, Pid, Reason} ->
            [{error, Reason} | collect(Next, Timeout)]
    after Timeout ->
        erlang:error({pmap_timeout, Timeout})
    end.