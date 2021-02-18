-module(api_router).
-behavior(gen_server).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2]).

-export([get_handler/3,
	 get_prefix/0,
	 get_authinfo/0,
	 get_routes/0]).

-record(state, {prefix,
		routes,
		authinfo}).

-record(route, {pattern,
		method,
		module,
		func,
		access}).

%%-----------------------------------------
%% RESTful Uri Path로 Handler를 검색
%% Path -> GET:/some/path/for/{id} 의 형태
%% Method -> "GET", "POST", "PUT", "DELETE"의 문자열
%% Heads -> Request Header 값 (인증용도???)
%%-----------------------------------------
get_handler(Path, Method, Headers) ->
    Match = string:lowercase(Method) ++ ":" ++ Path,
    Routes = gen_server:call(?MODULE, routes),
    %% Method는 "POST", "GET" 문자열이고 Router 설정에는
    %% get, post 등 atom형대로 정의 되었음.
    case find_route(Routes, Match) of
	nomatch ->
	    nomatch;
	{Route, PathParams} ->
	    logger:debug("access:~p", [Route#route.access]),
	    case check_auth(Route#route.access, Headers) of
		{unauthorized, Reason} ->
		    {unauthorized, Reason};
		UserInfo ->
		    {Route#route.module, Route#route.func, PathParams, UserInfo}
	    end
    end.

get_prefix() ->
    gen_server:call(?MODULE, prefix).

get_authinfo() ->
    gen_server:call(?MODULE, authinfo).

get_routes() ->
    gen_server:call(?MODULE, routes).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------
%% 라우터 설정 파일을 읽어 라우터 내용을
%% 상태데이터로 설정한다.
%%-----------------------------------------
init([]) ->
    {ok, Path} = application:get_env(routes),
    {ok, Json} = file:read_file(Path),
    Terms = misclib:json_to_terms(Json),

    AuthInfo = proplists:get_value(<<"authentication">>, Terms),
    Prefix = proplists:get_value(<<"prefix">>, Terms),
    Routes = proplists:get_value(<<"routes">>, Terms),
    RouteList = flatten(1, [<<"paths">>, <<"methods">>], Routes, [], []),
    RouteRecords = make_pattern(RouteList, Prefix, []),

    {ok, #state{prefix=Prefix,
		routes=RouteRecords,
		authinfo=AuthInfo}}.


handle_call(routes, _, State) ->
    {reply, State#state.routes, State};
handle_call(prefix, _, State) ->
    {reply, State#state.prefix, State};
handle_call(authinfo, _, State) ->
    {reply, State#state.authinfo, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_auth(undefined, _) ->
    [{auth_type, guest}];
check_auth(<<"guest">>, _) ->
    [{auth_type, guest}];
check_auth(<<"membership">>, Header) ->
    Info = get_authinfo(),
    Key = proplists:get_value(<<"key">>, Info),
    case misclib:token_decode(Header, Key) of
	{error, Reason} ->
	    {unauthorized, Reason};
	Value ->
	    logger:debug("checkauth:~p", [Value]),
	    Value
    end;
check_auth(Access, _) ->
    logger:error("binary no matching:~p", [Access]),
    {unauthorized, invalid_access_setting}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
make_pattern([H|T], Prefix, Acc) ->
    Access = proplists:get_value(<<"access">>, H),
    Module = proplists:get_value(<<"module">>, H),
    Method = proplists:get_value(<<"method">>, H),
    Path = proplists:get_value(<<"path">>, H),
    Func = proplists:get_value(<<"func">>, H),
    PathRe = re:replace(Path, "{([a-z_]+)}", "(?<\\g{1}>[^/,? ]+)", [global, {return, list}]),
    Chrs = io_lib:format("^~s:~s~s$", [Method, Prefix, PathRe]),
    {ok, Pattern} = re:compile(Chrs),
    Route = #route{pattern=Pattern,
		   method=Method,
		   module=Module,
		   func=Func,
		   access=Access},
    make_pattern(T, Prefix, [Route|Acc]);
make_pattern([], _, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
find_route([Route=#route{pattern=Pattern}|T], Subject) ->
    case re:run(Subject, Pattern, [{capture, all_names, binary}]) of
	{match, Captured} ->
	    {namelist, Names} = re:inspect(Pattern, namelist),
	    PathParams = lists:zip(Names, Captured),
	    {Route, PathParams};
	match ->
	    {Route, []};
	_ ->
	    %% nomatch | {error, Type}
	    find_route(T, Subject)
    end;
find_route([], _) ->
    nomatch.

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
flatten(Nth, Paths, [H|T], Cols, Rcds) when length(Paths) >= Nth ->
    Next = lists:nth(Nth, Paths),
    NewCols = lists:append([Cols, proplists:delete(Next, H)]),
    NextGen = proplists:get_value(Next, H),
    flatten(Nth, Paths, T, Cols, flatten(Nth+1, Paths, NextGen, NewCols, Rcds));
flatten(Nth, Paths, [H|T], Cols, Rcds) when length(Paths) < Nth ->
    NewCols = lists:append([Cols, H]),
    %% 마지막 T 노드 처리
    flatten(Nth, Paths, T, Cols, [NewCols|Rcds]);
flatten(_, _, [], _, Rcds) ->
    Rcds.
