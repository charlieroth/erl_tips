-module(ping).
-export([run/1]).

run(Host) ->
    Cmd = io_lib:format("ping -c 1 ~s", [Host]),
    Port = open_port({spawn, Cmd}, [binary, stderr_to_stdout]),
    receive
        {Port, {data, PingResponse}} ->
            parse_ping_response(Host, PingResponse)
    after 5000 ->
        ErrorMessage = list_to_binary(io_lib:format("Ping to ~s timed out after 5 seconds", [Host])),
        {error, ErrorMessage}
    end.

parse_ping_response(Host, PingResponse) ->
    ResponseParts = string:split(PingResponse, " ", all),
    ResponseTimeString = lists:filter(fun(Part) ->
        string:find(Part, "time=") /= nomatch
    end, ResponseParts),
    [_, ResponseTime] = string:split(ResponseTimeString, "="),
    Message = list_to_binary(io_lib:format("It took ~sms to ping ~s", [ResponseTime, Host])),
    {ok, Message}.