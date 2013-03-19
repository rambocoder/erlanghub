-module(github_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([publish/2]).

-define(INTERVAL, 10000). % Ten seconds

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  erlang:send_after(?INTERVAL, self(), trigger),
  {ok, queue:from_list(lists:seq(0, 3000))}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(trigger, Cache) ->
  {ok, Username} = application:get_env(erlanghub, username),
  {ok, Password} = application:get_env(erlanghub, password),
  case httpc:request(get, {"https://api.github.com/events", [auth_header(Username, Password)]}, [{timeout, 10000}], []) of
    {error, Reason} ->
      io:format("github_server error in https:request:~p~n", [Reason]),
      NewCache = Cache;
    {ok, {{_Version, StatusCode, _Reason}, Headers1, Body1}} ->
% process the content of JSON data
      case catch jiffy:decode(Body1) of
        {'EXIT', Reason} ->
          io:format("Caught 'EXIT': ~p~n", [Reason]),
          NewCache = Cache;
        {error, Reason} ->
          io:format("Caught error: ~p~n", [Reason]),
          NewCache = Cache;
        Events ->
          NewCache = lists:foldl(fun (Event, Acc) -> insert_event(Event, Acc) end, Cache, lists:reverse(Events)),
          io:format("Received Events from github:~n", []),
% ok = file:write_file("output.txt", Body1, [write, append]),
          RemainingHits = proplists:get_value("x-ratelimit-remaining", Headers1),
          io:format("Remaining hits: ~p ~n", [RemainingHits])
      end
  end,
  erlang:send_after(?INTERVAL, self(), trigger),
  {noreply, NewCache}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

insert_event(Event, Cache) ->
  {E} = Event,
  EventId = proplists:get_value(<<"id">>, E),
  case queue:member(EventId, Cache) of
    true -> Cache;
    false ->

      {Repo} = proplists:get_value(<<"repo">>, E),
      RepoUrl = proplists:get_value(<<"url">>, Repo),
      RepoId = proplists:get_value(<<"id">>, Repo),
      process_repo({RepoId, RepoUrl, Event}),
% remove the first element from the que
      Q1 = queue:drop(Cache),
% % add as the last element to the que
      queue:snoc(Q1, EventId)
  end.

process_repo({RepoId, RepoUrl, Event}) when is_number(RepoId) ->
  repo_server:repo_fetch({RepoId, RepoUrl}),
% check if the event is for an Erlang containing repo
  repo_server:check_repo(RepoId, Event, fun github_server:publish/2);
process_repo({RepoId, RepoUrl, Event}) ->
% io:format("Repo ~p does not have a number for its RepoId ~p for Event ~p~n", [RepoUrl, RepoId, Event]),
  ok.

publish([], _) ->
  nothing;
publish([{<<"TRUE">>}], E) ->
  io:format("Erlang event publish~p~n", [E]),
  tinymq:push("events", jiffy:encode(E));
publish([{<<"FALSE">>}], E) ->
  ok.


auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([erlang:atom_to_list(User),":",erlang:atom_to_list(Pass)])),
    {"Authorization","Basic " ++ Encoded}.