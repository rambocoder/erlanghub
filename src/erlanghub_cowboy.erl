-module(erlanghub_cowboy).

-export([start/0]).

start() ->
  application:start(ranch),
  application:start(cowboy),

  Dispatch = cowboy_router:compile([
      {'_', [
          {"/static/[...]", cowboy_static, [{directory, {priv_dir, erlanghub, "static"}}]},
          {"/pull", erlanghub_pull_handler, []},
          {'_', default_handler, []}
      ]}
  ]),
    {ok, _} = cowboy:start_http(http, 100,
                    [{port, 19860}], [{env, [{dispatch, Dispatch}]}]
  ),
  ok.
