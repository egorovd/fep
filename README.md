fep
=====

Fast Erlang Pool on NIF lock-free Queue using C++ the library: [moodycamel::concurrentqueue](https://github.com/cameron314/concurrentqueue/tree/8f7e861dd9411a0bf77a6b9de83a47b3424fafba)

#### moodycamel::ConcurrentQueue

An industrial-strength lock-free queue for C++.

**Features**:

* Knock-your-socks-off blazing fast performance.
* Single-header implementation. Just drop it in your project.
* Fully thread-safe lock-free queue. Use concurrently from any number of threads.
* C++11 implementation -- elements are moved (instead of copied) where possible.
* Templated, obviating the need to deal exclusively with pointers -- memory is managed for you.
* No artificial limitations on element types or maximum count.
* Memory can be allocated once up-front, or dynamically as needed.
* Fully portable (no assembly; all is done through standard C++11 primitives).
* Supports super-fast bulk operations.
* Includes a low-overhead blocking version (BlockingConcurrentQueue).
* Exception safe.


#### How it works.

Sometimes you need a pool of workers when: one worker - one operation (for example, processing one connection to the server).

Build
-----

    $ rebar3 compile

Using
-----

```erlang
  {ok, Ref} = fep:create_pool({fep_example, 1}),         % Create a pool.
  {ok, Pid} = supervisor:start_child(worker_sup, [Ref]),

  % In worker.
  % fep:push(PoolRef)
  % They created workers, each worker put himself in the pool.

  Pid = fep:pop(Ref).      % We take the worker from the pool.
  worker:run(Pid)          % We call the worker.

  % The worker after processing the job, puts himself in the pool.
  % fep:push(PoolRef)

  % kill worker
  exit(Pid, normal).

  % We can check if the worker is alive before giving:
  empty = fep:pop_if_alive(Ref).

  % Or if pool not empty and process is dead.
  dead = fep:pop_if_alive(Ref).
```

 Small example app: [fep_example](https://github.com/egorovd/fep_example)