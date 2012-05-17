-module(github).
-export([start/0, stop/0, loop/2, repo/0]).

start() ->
	crypto:start(),
	inets:start(),
	ssl:start(),
	Pid = erlang:spawn_link(fun github:repo/0),
	register(repo_server, Pid),
	sqlite3:open(ct, [{file, "/home/alex/Downloads/erlang_github/hello.sqlite"}]),
	stop = loop(1, queue:from_list(lists:seq(0, 3000))),
	stop().

stop() ->
	repo_server ! stop, 
	unregister(repo_server),
	sqlite3:close(ct).
	
repo() ->
	receive
		stop -> ok;
		{RepoId, RepoUrl} -> 	
				% check if the repo exists in the database
				case is_number(RepoId) of
					false -> repo();
					true -> continue
				end,
				Result = sqlite3:sql_exec(ct, "SELECT * FROM repos WHERE id = ?", [RepoId]),
				case length(proplists:get_value(rows, Result)) of
					1 -> io:format("Cache hit~n"), repo();
					_ -> not_found
				end,
				LanguageUrl = string:concat(unicode:characters_to_list(RepoUrl), "/languages"),
				% io:format("Language URL: ~p~n", [LanguageUrl]),
				% io:format("Repo id: ~p~n", [RepoId]),
				case httpc:request(LanguageUrl) of
					{error, Reason} -> io:format("Repo error:~p~n", [Reason]);
					{ok, {{_Version, StatusCode, _Reason}, Headers1, Body1}} -> 
						case StatusCode of
							200 ->
									{Languages} = jiffy:decode(Body1),
									Erlang = case proplists:is_defined(<<"Erlang">>, Languages) of
										true -> "TRUE";
										false -> "FALSE"
									end,
				
									sqlite3:sql_exec(ct, "INSERT INTO repos (id, URL, Erlang, JSON) VALUES (?, ?, ?, ?)", 
											[RepoId, 
											RepoUrl, 
											Erlang,
											Body1]);
							_ -> nothing
						end
				end,
				
				timer:sleep(1000),
				repo()
	end.

loop(IterationCount, Cache) when IterationCount < 10000 ->
	% timer:sleep(1000),
	Url = "https://api.github.com/events",
	{ok, {Status1, Headers1, Body1}} = httpc:request(Url),
	% process the content of JSON data
	case catch jiffy:decode(Body1) of
		{'EXIT', Reason } -> io:format("Caught 'EXIT': ~p~n", [Reason]), Events = [], timer:sleep(10000), loop(IterationCount + 1, Cache);
		{error, Reason } -> io:format("Caught error: ~p~n", [Reason]), Events = [], timer:sleep(10000), loop(IterationCount + 1, Cache);
		Events -> Events
	end,
	{S3, Repeats} = lists:foldl(fun(Event, Acc) -> insert_event(Event, IterationCount, Acc) end, {Cache , 0}, lists:reverse(Events)),

	% ok = file:write_file("output.txt", Body1, [write, append]),
	RemainingHits = proplists:get_value("x-ratelimit-remaining", Headers1),
	io:format("Remaining hits: ~p with repeats: ~p~n", [RemainingHits, Repeats]),
	io:format("Start ~p iteration~n", [IterationCount]),
	timer:sleep(10000),
	loop(IterationCount + 1, S3);
	
loop(_, _) ->
	stop.

insert_event(Event, Iteration, {Cache, Repeats}) ->
	{E} = Event,
	EventId = proplists:get_value(<<"id">>, E),
	case queue:member(EventId, Cache) of
		true ->	io:format("Repeat ~p ~n", [Repeats+1]),  {Cache, Repeats + 1};
		false -> 

				{Repo} = proplists:get_value(<<"repo">>, E),
				RepoUrl = proplists:get_value(<<"url">>, Repo),
				RepoId = proplists:get_value(<<"id">>, Repo),
				RepoId1 = case is_number(RepoId) of
					false -> 0;
					true -> RepoId
				end,
				sqlite3:sql_exec(ct, "INSERT INTO github6 (id, github_time, pull_time, iteration, repo_id) VALUES (?, ?, ?, ?, ?)", 
						[EventId, 
						proplists:get_value(<<"created_at">>, E), 
						httpd_util:rfc1123_date(erlang:localtime()),
						Iteration,
						RepoId1]),
								
				repo_server ! {RepoId, RepoUrl},
				% check if the event for a possible erlang repo
				check_repo(RepoId, E),
				Q1 = queue:drop(Cache), % remove the first element from the que % add as the last element to the que
				{queue:snoc(Q1, EventId) , Repeats}
	end.

check_repo(RepoId, E) when is_number(RepoId) ->
	Result = sqlite3:sql_exec(ct, "SELECT erlang FROM repos WHERE id = ?", [RepoId]),
	Rows = proplists:get_value(rows, Result),
	publish(Rows, E);
check_repo(_, _) -> nothing.

publish([], _) ->
	nothing;
publish([{<<"TRUE">>}], E) ->
	io:format("Erlang event ~p~n", [E]);
publish([{<<"FALSE">>}], _) ->
	nothing.	
