-module(busy_beam).
-export([top_n_reduction_pids/1, top_n_memory_pids/1, all_processes/0]).

top_n_reduction_pids(TopN) ->
    TopNBy = top_n_by(TopN, reductions),
    io:format("Top 5 reduction count processes:~n~s~n", [TopNBy]).

top_n_memory_pids(TopN) ->
    TopNBy = top_n_by(TopN, heap_size),
    io:format("Top 5 heap size processes:~n~s~n", [TopNBy]).

top_n_by(TopN, Field) ->
    AllProcesses = all_processes(),
    SortedByField = lists:sort(fun({_Pid1, Info1}, {_Pid2, Info2}) ->
        maps:get(Field, Info1) > maps:get(Field, Info2)
    end, AllProcesses),
    Enumerated = lists:enumerate(SortedByField),
    TopProcesses = lists:takewhile(fun({Index, _}) -> Index < TopN end, Enumerated),
    Messages = lists:map(fun({_Index, {Pid, PidInfo}}) ->
        InfoRegisteredName = process_info(Pid, registered_name),
        Measurement = maps:get(Field, PidInfo),
        case InfoRegisteredName of
            {registered_name, Name} ->
                io_lib:format("~p -> ~p", [Name, Measurement]);
            [] ->
                io_lib:format("~p -> ~p", [Pid, Measurement]);
            undefined ->
                undefined
        end
    end, TopProcesses),
    [[M,"\n"] || M <- Messages].

all_processes() ->
    lists:map(fun(Pid) ->
        ProcessInfoList = process_info(Pid),
        ProcessInfoMap = maps:from_list(ProcessInfoList),
        {Pid, ProcessInfoMap}
    end, processes()).