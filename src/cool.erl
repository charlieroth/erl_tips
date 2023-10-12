-module(cool).
-export([start/0, hello/0, foo/0]).

start() ->
    Hello = hello(),
    Foo = foo(),
    io:format("~s ~s~n", [Hello, Foo]).

hello() -> "world".

foo() -> "bar".