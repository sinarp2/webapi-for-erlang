-module(handler_auth).

-export([login/1, logout/1, signup/1]).

-include("macros.hrl").

signup(Model) ->
    %% POST
    %% email, username, password
    Username = Model(param, "username"),
    Password = Model(param, "password"),
    Email = Model(param, "email"),

    if
	Username == undefined ->
	    throw(username_not_found);
	Password == undefined ->
	    throw(password_not_found);
	Email == undefined ->
	    throw(email_not_found);
	true ->
	    ok
    end,

    Query = [{query,
	      [{term,
		[{<<"email.keyword">>, Email}]}]}],

    Hits = es:search(<<"users">>, Query),
    if
	length(Hits) > 0 ->
	    throw(duplicated_email);
	true ->
	    ok
    end,

    Hash = crypto:hash(sha256, Password),
    Doc = [{first_name, Username},
	   {last_name, <<"">>},
	   {company_name, <<"">>},
	   {address, <<"">>},
	   {city, <<"">>},
	   {county, <<"">>},
	   {state, <<"">>},
	   {zip, <<"">>},
	   {phone1, <<"">>},
	   {phone2, <<"">>},
	   {email, Email},
	   {web, <<"">>},
	   {password, Hash}],

    case es:insert(<<"users">>, <<"_doc">>, Doc) of
	{fail, Reason} ->
	    throw(Reason);
	_ ->
	    Model(put, {result, success}),
	    Model(put, {email, Email})
    end.

logout(Model) ->
    Model(put, {result, success}).

login(Model) ->
    %% 포스트 데이터인 경우 바이너리 형태로 파싱이되어 넘어옴.
    %% TODO:
    %% 포스트, 풋 인 경우 사용자가 알아서 바이너리 행태로 조회할 것인가?
    %% 포스트, 풋 인 경우 자동으로 바이너리로 변환해 줄 것인가?

    UserId = Model(param, "userid"),
    if
	UserId == undefined ->
	    throw(userid_not_found);
	true ->
	    ok
    end,

    Query = [{size, 5},
	     {from, 0},
	     {<<"_source">>, false},
	     {fields, [<<"first_name">>,
		       <<"last_name">>,
		       <<"email">>]},
	     {query, [{bool,
		       [{must,
			 [{term,
			   [{'email.keyword', UserId}]}]}]}]}],

    Hits = es:search(<<"users">>, Query),
    if
	length(Hits) == 0 ->
	    throw(user_not_found);
	length(Hits) > 1 ->
	    throw(multiple_users);
	true ->
	    ok
    end,

    [UserData] = Hits,
    Id = ?prop(<<"_id">>, UserData),
    Fields = ?prop(<<"fields">>, UserData),
    Firstname = ?prop(<<"first_name">>, Fields),
    Lastname = ?prop(<<"last_name">>, Fields),
    Email = ?prop(<<"email">>, Fields),
    Username = iolist_to_binary([Firstname, <<" ">>, Lastname]),
    _Password = Model(param, "password"),
    %% compare password parameter with stored password

    {_Type, Config} = api_router:get_authinfo(),
    Alg = ?prop(algorithm, Config),
    Expiration = ?prop(expiration, Config),
    Key = ?prop(key, Config),
    Claims = [{email, Email},
	      {username, Username}],
    case misclib:token_encode(Claims, Alg, Expiration, Key) of
	{ok, Token} ->
	    UpTo = erlang:system_time(second) + Expiration,
	    %%{{Y,M,D}, {H,Mi,S}} = calendar:gregorian_seconds_to_datetime(UpTo),
	    Model(put, {result, success}),
	    Model(put, {id, Id}),
	    Model(put, {token, Token}),
	    Model(put, {username, Username}),
	    Model(put, {email, Email}),
	    Model(put, {expires, UpTo});
	{error, Reason} ->
	    Model(put, {result, fail}),
	    Model(put, {reason, Reason})
    end.
