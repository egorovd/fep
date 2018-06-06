-module(fep_queue).

%% API exports
-export([new/0, push/1, pop/1, pop_if_alive/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec(new() -> {ok, QueueRef :: reference()} | badarg | {error, Reason :: binary()}).
new() ->
  fep_nif:new().

-spec(push(QueueRef :: reference()) ->
  true | {error, Reason :: binary()}).
push(QueueRef) ->
  fep_nif:push(QueueRef).

-spec(pop(QueueRef :: reference()) ->
  {ok, pid()} | empty | {error, Reason :: binary()}).
pop(QueueRef) ->
  fep_nif:pop(QueueRef).

-spec(pop_if_alive(QueueRef :: reference()) ->
  {ok, pid()} | empty | dead | {error, Reason :: binary()}).
pop_if_alive(QueueRef) ->
  fep_nif:pop_if_alive(QueueRef).

%%====================================================================
%% Internal functions
%%====================================================================
