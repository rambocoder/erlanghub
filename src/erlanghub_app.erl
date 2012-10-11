-module(erlanghub_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ok = crypto:start(),
  ok = inets:start(),
  ok = ssl:start(),
  {ok, _} = sqlite3:open(ct, [{file, "priv/erlanghub.sqlite"}]),
  ok = application:start(tinymq),
  ok = erlanghub_cowboy:start(),
  erlanghub_sup:start_link().

stop(_State) ->
  ok.
