-module(handler_auth).

-export([login/1, logout/1, signup/1]).

signup(Model) ->
    %% POST
    %% email, username, password
    DisplayName = Model(param, <<"displayName">>),
    Email = Model(param, <<"email">>),
    Password = Model(param, <<"password">>),

    if
	DisplayName == undefined ->
	    throw({400, display_name_not_found});
	Password == undefined ->
	    throw({400, password_not_found});
	Email == undefined ->
	    throw({400, email_not_found});
	true ->
	    ok
    end,

    Query = [{query,
	      [{term,
		[{<<"email.keyword">>, Email}]}]}],

    Hits = es:search(<<"users">>, Query),
    if
	length(Hits) > 0 ->
	    throw({400, duplicated_email});
	true ->
	    ok
    end,

    %% binary()는 json 변환 과정에서 encoding 되기 때문에
    %% 해시 결과binary()를 그대로 문자화 시킨 후 다시 바이너리 문자여로 만들어야
    %% 원래의 행태가 유지됨.
    %% <<123,123,123...>> -> "<<123,123,123...>>" -> <<"<<123,123,123...>>">>
    %% binary() -> binary를 그대로 문자화 -> 문자화된 것을 다시 바이너리로
    Hash = crypto:hash(md5, Password),
    HashStr = list_to_binary(io_lib:format("~p", [Hash])),
    Doc = [{display_name, DisplayName},
	   {email, Email},
	   {password, HashStr}],

    case es:insert(<<"users">>, <<"_doc">>, Doc) of
	{fail, Reason} ->
	    throw({401, Reason});
	Res ->
	    Id = proplists:get_value(<<"_id">>, Res),
	    case issue_token(Model, Id, Email, DisplayName) of
		ok ->
		    ok;
		Reason ->
		    logger:error("token failure:~p", [Reason]),
		    throw({401, issue_token_failure})
	    end
    end.

logout(Model) ->
    Model(put, {result, success}).

login(Model) ->
    %% 포스트 데이터인 경우 바이너리 형태로 파싱이되어 넘어옴.
    %% TODO:
    %% 포스트, 풋 인 경우 사용자가 알아서 바이너리 행태로 조회할 것인가?
    %% 포스트, 풋 인 경우 자동으로 바이너리로 변환해 줄 것인가?

    ParamEmail = Model(param, <<"email">>),
    if
	ParamEmail == undefined ->
	    throw({400, email_not_found});
	true ->
	    ok
    end,

    Query = [{size, 5},
	     {from, 0},
	     {<<"_source">>, false},
	     {fields, [<<"display_name">>,
		       <<"password">>,
		       <<"email">>]},
	     {query, [{bool,
		       [{must,
			 [{term,
			   [{'email.keyword', ParamEmail}]}]}]}]}],

    Hits = es:search(<<"users">>, Query),
    if
	length(Hits) == 0 ->
	    throw({401, user_not_found});
	length(Hits) > 1 ->
	    throw({401, multiple_users});
	true ->
	    ok
    end,

    [UserData] = Hits,
    Id = proplists:get_value(<<"_id">>, UserData),
    FieldData = proplists:get_value(<<"fields">>, UserData),
    TupleList = [{K, hd(V)} || {K, V} <- FieldData],
    DisplayName = proplists:get_value(<<"display_name">>, TupleList),
    StoredPassword = proplists:get_value(<<"password">>, TupleList),
    Email = proplists:get_value(<<"email">>, TupleList),

    Password = Model(param, <<"password">>),
    Hash = crypto:hash(md5, Password),
    HashStr = list_to_binary(io_lib:format("~p", [Hash])),
    %% compare password parameter with stored password
    if
	StoredPassword /= HashStr ->
	    throw({401, invalid_password});
	true ->
	    ok
    end,

    case issue_token(Model, Id, Email, DisplayName) of
	ok ->
	    ok;
	Reason ->
	    logger:error("token failure:~p", [Reason]),
	    throw({401, issue_token_failure})
    end.

issue_token(Model, Id, Email, DisplayName) ->
    Props = api_router:get_authinfo(),
    Alg = proplists:get_value(<<"algorithm">>, Props),
    Expiration = proplists:get_value(<<"expiration">>, Props),
    Key = proplists:get_value(<<"key">>, Props),
    Claims = [{email, Email},
	      {display_name, DisplayName}],
    case misclib:token_encode(Claims, Alg, Expiration, Key) of
	{ok, Token} ->
	    UpTo = erlang:system_time(second) + Expiration,
	    %%{{Y,M,D}, {H,Mi,S}} = calendar:gregorian_seconds_to_datetime(UpTo),
	    Model(put, {result, success}),
	    Model(put, {id, Id}),
	    Model(put, {token, Token}),
	    Model(put, {displayName, DisplayName}),
	    Model(put, {email, Email}),
	    Model(put, {expires, UpTo}),
	    ok;
	{error, Reason} ->
	    Reason
    end.
