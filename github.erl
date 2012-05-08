-module(github).
-export([start/0, loop/1]).

start() ->
	crypto:start(),
	inets:start(),
	ssl:start(),
	loop("1").

loop(PageNumber) ->
	%timer:sleep(1000),
	InitialUrl = "https://api.github.com/events?page=",
	NextUrl = string:concat(InitialUrl, PageNumber),
	{ok, {Status1, Headers1, Body1}} = httpc:request(get, {NextUrl, []}, [], []),
	% process the content of JSON data
	ok = file:write_file("output.txt", Body1, [write, append]),
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

