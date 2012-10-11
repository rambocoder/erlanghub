-module(erlanghub_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ok = erlanghub_cowboy:start(),
  ok = crypto:start(),
  ok = inets:start(),
  ok = ssl:start(),
  sqlite3:open(ct, [{file, "priv/erlanghub.sqlite"}]),
  application:start(tinymq),
  erlanghub_sup:start_link().

stop(_State) ->
  ok.
