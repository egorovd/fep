-module(fep).

%% API exports

-export([create_pool/1, delete_pool/1]).
-export([push/1, pop/1, pop_if_alive/1]).
-export([lookup_ref/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec(create_pool(Key :: any()) ->
  {ok, QueueRef :: reference()} | {exist, QueueRef :: reference()} | badarg | {error, Reason :: binary()}).
create_pool(Key) ->
  fep_srv:create_pool(Key).

-spec(delete_pool(Key :: any()) ->
  ok | {error, Reason :: binary()}).
delete_pool(Key) ->
  fep_srv:delete_pool(Key).

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

-spec(lookup_ref(Key :: any()) ->
  Ref :: reference() | not_found).
lookup_ref(Key) ->
  fep_srv:lookup_ref(Key).

%%====================================================================
%% Internal functions
%%====================================================================
