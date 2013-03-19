-module(erlanghub_pull_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {Since, Req3} = cowboy_req:qs_val(<<"since">>, Req2),
  {ok, Req4} = since(Method, Since, Req3),
  {ok, Req4, State}.

since(<<"GET">>, undefined, Req) ->
  Timestamp = tinymq:now("events"),
  Payload = jiffy:encode({[{timestamp, Timestamp}]}),
  cowboy_req:reply(200, [], Payload, Req);

since(<<"GET">>, Since, Req) ->
  Integer = list_to_integer(binary_to_list(Since)),
  io:format("TimeStamp coming from pull: ~p~n", [Integer]),
  tinymq:subscribe("events", Integer, self()),
  receive
    {_From, Timestamp, Messages} ->
      io:format("Received messages: ~p with timestamp: ~p~n", [Messages, Timestamp]),
      Timeout = <<"false">>
  after 15000 ->
    io:format("Timedout with timestamp ~p~n", [Integer]),
    Messages = [<<"">>],
    Timeout = <<"true">>,
    Timestamp = Since
  end,
  Json = jiffy:encode({[{timestamp, Timestamp}, {messages, Messages}, {timeout, Timeout}]}),
  cowboy_req:reply(200,
                   [{<<"content-encoding">>, <<"utf-8">>}, {<<"content-type">>, <<"application/json">>}], Json, Req);
since(_, _, Req) ->
%% Method not allowed.
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
  ok.