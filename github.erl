-module(github).
-export([start/0, loop/1]).

start() ->
	crypto:start(),
	inets:start(),
	ssl:start(),
	sqlite3:open(ct, [{file, "/home/alex/Downloads/erlanghub/erlang-sqlite3/hello.sqlite"}]),
	loop("1").

loop(PageNumber) ->
	timer:sleep(1000),
	InitialUrl = "https://api.github.com/events?page=",
	NextUrl = string:concat(InitialUrl, PageNumber),
	{ok, {Status1, Headers1, Body1}} = httpc:request(NextUrl),
	% process the content of JSON data
	Obj = mochijson2:decode(Body1),
	Events = proplists:get_all_values(struct, Obj),
	lists:foreach(fun(Event) ->  insert_event(Event) end, Events),
	% ok = file:write_file("output.txt", Body1, [write, append]),
	Link = proplists:get_value("link", Headers1),
	case string:str(Link, "next") of
		0 -> 		io:format("Start from page 1~n"),
					loop("1");
		_ -> 		LinkStart = string:str(Link, "=") + 1,
					LinkEnd = string:str(Link, ">") -1,
					NextPageNumber = string:sub_string(Link, LinkStart, LinkEnd),
					io:format("Next page: ~p~n", [NextPageNumber]),
					RemainingHits = proplists:get_value("x-ratelimit-remaining", Headers1),
					io:format("Remaining hits: ~p~n", [RemainingHits]),
					loop(NextPageNumber)
	end.

insert_event(Event) ->
	io:format("~p~n", [proplists:get_value(<<"id">>, Event)]),
	io:format("~p~n", [proplists:get_value(<<"created_at">>, Event)]),
	sqlite3:sql_exec(ct, "INSERT INTO github (id, github_time, pull_time) VALUES (?, ?, ?)", 
			[proplists:get_value(<<"id">>, Event), proplists:get_value(<<"created_at">>, Event), httpd_util:rfc1123_date(erlang:localtime())]).
