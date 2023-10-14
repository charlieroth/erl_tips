-module(nav).
-behaviour(gen_statem).
-export([init/1, callback_mode/0, handle_event/4]).
-export([start/0, turn_left/0, turn_right/0, status/0]).

turn_left() -> gen_statem:cast(?MODULE, turn_left).

turn_right() -> gen_statem:cast(?MODULE, turn_right).

status() ->
    {Direction, TurnCount} = sys:get_state(?MODULE),
    io:format("You are currently traveling ~p bound and have made ~p turn(s)~n", [Direction, TurnCount]).

start() -> start_link().

start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, north, 0}.

callback_mode() -> [handle_event_function].

handle_event(cast, turn_left, north, TurnCount) -> {next_state, west, TurnCount + 1};
handle_event(cast, turn_left, east, TurnCount) -> {next_state, north, TurnCount + 1};
handle_event(cast, turn_left, south, TurnCount) -> {next_state, east, TurnCount + 1};
handle_event(cast, turn_left, west, TurnCount) -> {next_state, south, TurnCount + 1};

handle_event(cast, turn_right, north, TurnCount) -> {next_state, east, TurnCount + 1};
handle_event(cast, turn_right, east, TurnCount) -> {next_state, south, TurnCount + 1};
handle_event(cast, turn_right, south, TurnCount) -> {next_state, west, TurnCount + 1};
handle_event(cast, turn_right, west, TurnCount) -> {next_state, north, TurnCount + 1}.