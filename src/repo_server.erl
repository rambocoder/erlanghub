-module(repo_server).

-behaviour(gen_server).

-export([start_link/0, repo_fetch/1, fetch_language_info/1, check_repo/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API functions
repo_fetch({RepoId, RepoUrl}) ->
  gen_server:cast(?MODULE, {fetch_language, RepoId, RepoUrl}).

check_repo(RepoId, Event, PublishFun) ->
  gen_server:cast(?MODULE, {new_event, RepoId, Event, PublishFun}).

%% gen_server functions

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, [queue:from_list(lists:seq(0, 3000))]}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({fetch_language, RepoId, RepoUrl}, State) ->
% check if the repo exists in the database
  Result = sqlite3:sql_exec(ct, "SELECT * FROM repos WHERE id = ?", [RepoId]),
  case length(proplists:get_value(rows, Result)) of
    1 -> io:format("Repo cache hit~n");
    _ -> fetch_language_info({RepoId, RepoUrl})
  end,
  {noreply, State};

handle_cast({new_event, RepoId, Event, PublishFun}, State) ->
  Result = sqlite3:sql_exec(ct, "SELECT ShowAsErlang FROM repos WHERE id = ?", [RepoId]),
  Rows = proplists:get_value(rows, Result),
  PublishFun(Rows, Event),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



fetch_language_info({RepoId, RepoUrl}) ->
  LanguageUrl = string:concat(unicode:characters_to_list(RepoUrl), "/languages"),
  case httpc:request(get, {LanguageUrl, []}, [{timeout, 10000}], []) of
    {error, Reason} -> io:format("Repo error:~p~n", [Reason]);
    {ok, {{_Version, StatusCode, _Reason}, _Headers1, Body1}} ->
      case StatusCode of
        200 ->
          {Languages} = jiffy:decode(Body1),
          IsErlang = case proplists:is_defined(<<"Erlang">>, Languages) of
            true -> "TRUE";
            false -> "FALSE"
          end,
% by default, all ContainsErlang are also ShowAsErlang
          sqlite3:sql_exec(ct, "INSERT INTO repos (id, URL, ContainsErlang, LanguagesJSON, PullTime, ShowAsErlang) VALUES (?, ?, ?, ?, ?, ?)",
                           [RepoId,
                            RepoUrl,
                            IsErlang,
                            Body1,
                            httpd_util:rfc1123_date(erlang:localtime()),
                            IsErlang]);
        _ -> nothing
      end
  end,
% io:format("Language URL: ~p~n", [LanguageUrl]),
% io:format("Repo id: ~p~n", [RepoId]),
  timer:sleep(100).
