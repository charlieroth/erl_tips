-module(two).
-export([named_captures/2]).

named_captures(RegExp, Subject) ->
    {ok, MP} = re:compile(RegExp),
    {namelist, Names} = re:inspect(MP, namelist),
    {match, Values} = re:run(Subject, RegExp, [{capture, Names, list}]),
    lists:zip(Names, Values).