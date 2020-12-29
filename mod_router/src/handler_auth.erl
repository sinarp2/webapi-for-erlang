-module(handler_auth).

-export([login/1, logout/1, signup/1]).

-include("macros.hrl").


signup(Model) ->
    Model(put, {result, ok}).

logout(Model) ->
    Model(put, {result, ok}).

login(Model) ->
    %% 포스트 데이터인 경우 바이너리 형태로 파싱이되어 넘어옴.
    %% TODO:
    %% 포스트, 풋 인 경우 사용자가 알아서 바이너리 행태로 조회할 것인가?
    %% 포스트, 풋 인 경우 자동으로 바이너리로 변환해 줄 것인가?
    logger:debug("userid:~p~n", [Model(param, "userid")]),
    logger:debug("password:~p~n", [Model(param, "password")]),
    Query = io_lib:format("{\"_source\":true,\"query\":{\"bool\":{\"must\":[{\"term\":{\"email.keyword\":\"~s\"}}]}}}",
			  [Model(param, "userid")]),
    case es:request("/users/_search", lists:flatten(Query)) of
	{ok, Resp} ->
	    login_check(Model, Resp);
	{error, Resp} ->
	    Model(put, {result, error}),
	    Model(put, {reson, jiffy:encode(Resp)})
    end.

%% jiffy로 decode 결과는 다음과 같은 erlang 데이터로 표현된다.
%% root   -> {[{}]} -> tuple:proplists
%% object -> {[{}]} -> tuple:proplists
%% list   -> [[{}]] -> list:proplists
login_check(Model, Resp) ->
    {Root} = jiffy:decode(Resp),
    {Hits} = ?prop(<<"hits">>, Root),
    case ?prop(<<"hits">>, Hits) of
	[] ->
	    Model(put, {result, <<"user not found">>});
	[{Hitss}] ->
	    {Source} = ?prop(<<"_source">>, Hitss),

	    Firstname = ?prop(<<"first_name">>, Source),
	    Lastname = ?prop(<<"last_name">>, Source),
	    Email = ?prop(<<"email">>, Source),
	    Username = iolist_to_binary([Firstname, <<" ">>, Lastname]),
	    Password = Model(param, "password"),
	    %% compare password parameter with stored password

	    {_Type, Config} = router_srv:get_authinfo(),
	    Key = ?prop(key, Config),
	    Expiration = ?prop(expiration, Config),
	    Claims = [{email, Email},
		      {username, Username}],
	    {ok, Token} = jwt:encode(<<"HS256">>, Claims, Expiration, Key),
	    UpTo = erlang:system_time(second) + Expiration,
	    {{Y,M,D}, {H,Mi,S}} = calendar:gregorian_seconds_to_datetime(UpTo),
	    logger:debug("expires at ~p-~p-~p ~2..0B:~2..0B:~2..0B",
			 [Y+1970,M,D,H,Mi,S]),
	    Result = {[{token, Token},
		       {username, Username},
		       {email, Email},
		       {expires, UpTo}]},
	    Model(put, {result, Result})
    end.
