-module(counter).
-version("2").

-export([start_link/0, incr/0, count/0]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{debug, [trace]}]).

incr()  -> gen_server:call(?MODULE, incr).
count() -> gen_server:call(?MODULE, count).

init([]) -> {ok, 0}.

handle_call(incr, _From, {OldState, State}) -> {reply, ok, {OldState, State+1}};
handle_call(count, _From, {OldState, State}) -> {reply, State, {OldState, State}};
handle_call(_Call, _From, State) -> {noreply, State}.

handle_cast(_Cast, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change("1", State, _Extra) -> {ok, {State, State}}.