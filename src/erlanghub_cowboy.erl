-module(erlanghub_cowboy).

-export([start/0]).

start() ->
  application:start(ranch),
  application:start(cowboy),

  Dispatch = [
      {'_', [
          %{[], home_handler, [<<"html">>, <<"index.html">>]},
          {[<<"static">>, '...'], cowboy_static, [{directory, {priv_dir, erlanghub, [<<"static">>]}}]},
          {[<<"pull">>], erlanghub_pull_handler, []},
          {[], default_handler, []}
      ]}
  ],
  cowboy:start_http(sample_http_handler, 100,
                    [{port, 19860}], [{dispatch, Dispatch}]
  ),
  ok.
