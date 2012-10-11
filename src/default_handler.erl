%% Feel free to use, reuse and abuse the code in this file.

-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path, Req2} = cowboy_req:path(Req1),
  io:format("Call handle ~p~p~n", [Method, Path]),
  {ok, Req3} = handle(Method, Path, Req2),
  {ok, Req3, State}.

handle(<<"GET">>, <<"/">>, Req) ->
  {ok, Bin} = file("html/index.html"),
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Bin, Req),
  {ok, Req2};

handle(<<"GET">>, <<"/favicon.ico">>, Req) ->
  {ok, Bin} = file("html/favicon.ico"),
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"image/x-icon">>}], Bin, Req),
  {ok, Req2}.

terminate(_Req, _State) ->
	ok.

file(Path) ->
  Priv = priv(),
  {ok, Bin} = file:read_file(filename:join(Priv, Path)),
  {ok, Bin}.

priv() ->
  case code:priv_dir(erlanghub) of
    {error, _} -> "priv";
    Priv -> Priv
  end.