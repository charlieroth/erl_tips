-module(runtime).
-export([runtime_info/0, runtime_info/1]).

runtime_info() ->
    runtime_info([system, memory, limits]).

runtime_info(Topic) when is_atom(Topic) ->
    runtime_info([Topic]);

runtime_info(Topics) when is_list(Topics) ->
    UniqueTopics = lists:uniq(Topics),
    print_runtime_info(UniqueTopics).

print_runtime_info(Topics) ->
    lists:foreach(fun(Topic) -> print_runtime_info_topic(Topic) end, Topics),
    io:format("~n"),
    print_topic_info(Topics),
    io:format("~n").

print_topic_info(Topics) when is_list(Topics) ->
    io:format("Showing topics~n"),
    io:format("~p~n", [Topics]),
    io:format("~n"),
    io:format("To view a specific topic call runtime:runtime_info(Topic).~n").

print_runtime_info_topic(system) ->
    print_pane("System and architecture"),
    print_entry("Erlang/OTP version", erlang:system_info(otp_release)),
    print_entry("ERTS version", erlang:system_info(version)),
    print_entry("Compiled for", erlang:system_info(system_architecture)),
    print_entry("Schedulers", erlang:system_info(schedulers)),
    print_entry("Schedulers online", erlang:system_info(schedulers_online));

print_runtime_info_topic(memory) ->
    print_pane("Memory"),
    print_memory("Total", total),
    print_memory("Atoms", atom),
    print_memory("Binaries", binary),
    print_memory("Code", code),
    print_memory("ETS", ets),
    print_memory("Processes", processes);

print_runtime_info_topic(limits) ->
    print_pane("Statistics / limits"),
    print_uptime(),
    print_entry("Run queue", statistics(run_queue)),
    print_percentage("Atoms", atom_count, atom_limit),
    print_percentage("ETS", ets_count, ets_limit),
    print_percentage("Ports", port_count, port_limit),
    print_percentage("Processes", process_count, process_limit).

print_pane(Msg) ->
    io:format("~n## ~s ~n", [Msg]).

print_entry(_Key, undefined) -> ok;
print_entry(Key, Value) -> io:format("~s ~p~n", [Key, Value]).

print_uptime() ->
    io:format("Uptime: "),
    c:uptime().

print_percentage(Key, Min, Max) ->
    Min1 = get_stat(Min),
    Max1 = get_stat(Max),
    Percentage = trunc(Min1 / Max1 * 100),
    io:format("~s ~p / ~p (~p% used)~n", [Key, Min1, Max1, Percentage]).

get_stat(ets_count) -> erlang:system_info(ets_count);
get_stat(Other) -> erlang:system_info(Other).

print_memory(Key, Memory) ->
    Value = erlang:memory(Memory),
    io:format("~s ~s~n", [Key, format_bytes(Value)]).

format_bytes(Bytes) when is_integer(Bytes) ->
    GbSize = memory_unit(gb),
    MbSize = memory_unit(mb),
    KbSize = memory_unit(kb),
    if
        Bytes >= GbSize -> 
            format_bytes(Bytes, gb);
        Bytes >= MbSize -> 
            format_bytes(Bytes, mb);
        Bytes >= KbSize -> 
            format_bytes(Bytes, kb);
        true -> 
            format_bytes(Bytes, byt)
    end.

format_bytes(Bytes, Unit) when is_integer(Bytes) ->
    BytesInMemUnit = Bytes / memory_unit(Unit),
    RoundedBytes = round(BytesInMemUnit),
    io_lib:format("~p ~p", [RoundedBytes, Unit]);

format_bytes(Bytes, byt) -> 
    io_lib:format("~p B", [Bytes]).

memory_unit(gb) -> 1024 * 1024 * 1024;
memory_unit(mb) -> 1024 * 1024;
memory_unit(kb) -> 1024.