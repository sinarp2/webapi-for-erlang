-module(router_srv).
-behavior(gen_server).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2]).

-export([get_handler/3]).

-record(state, {routes,
		authinfo}).

-record(route, {pattern,
		auth,
		path,
		method,
		module,
		handler,
		description}).

-include("macros.hrl").

%%-----------------------------------------
%% RESTful Uri Path로 Handler를 검색
%% Path -> GET:/some/path/for/{id} 의 형태
%% Method -> get, post, put, delete의 atom
%% Heads -> Request Header 값 (인증용도???)
%%-----------------------------------------
get_handler(Path, Method, _Heads) ->
    State = gen_server:call(?MODULE, get_routes),
    %% Method는 "POST", "GET" 문자열이고 Router 설정에는
    %% get, post 등 atom형대로 정의 되었음.
    case find_handler(atom_to_list(Method) ++ ":" ++ Path,
		      State#state.routes) of
	undefined ->
	    undefined;
	Handler ->
	    [Handler, State#state.authinfo]
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------
%% 라우터 설정 파일을 읽어 라우터 내용을
%% 상태데이터로 설정한다.
%%-----------------------------------------
init([]) ->
    {ok, Path} = application:get_env(routes),
    {ok, [InfoList]} = file:consult(Path),
    %%logger:debug("Route Info: ~p~n", [InfoList]),
    Prefix = ?prop(prefix, InfoList),
    Routes = ?prop(routes, InfoList),
    Auth = ?prop(authentication, InfoList),
    Catalog = build_catalog(Routes, [Prefix], []),
    %%logger:debug("RouteCatalog: ~p~n", [Catalog]),
    {ok, #state{routes=Catalog, authinfo=Auth}}.

handle_call(get_routes, _From, RouteData) ->
    {reply, RouteData, RouteData}.

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc RESTful URI Path로 Handler 검색을 위한 RegEx 생성
%% @spec
%% @end
%%--------------------------------------------------------------------
%% first depth : handler, auth, [path]
build_catalog([{Mod,Ath,Pths}|T], E, Acc)
  when is_list(Ath) ->
    build_catalog(T, E, build_catalog(Pths, [Ath|[Mod|E]], Acc));
%% second depth : path, [method]
build_catalog([{Pth,Mtds}|T], E, Acc) ->
    build_catalog(T, E, build_catalog(Mtds, [Pth|E], Acc));
%% third depth : [method, fun, description]
build_catalog([{Mtd,Fun,Dsc}|T], [Pth,Ath,Mod,Pfx]=E, Acc)
  when is_atom(Fun) ->
    {ok, Prn} = uri_pattern(Mtd, Pfx, Pth),
    Route = #route{pattern=Prn,
		   auth=Ath,
		   path=Pth,
		   method=Mtd,
		   module=Mod,
		   handler=Fun,
		   description=Dsc},
    build_catalog(T, E, [Route|Acc]);
build_catalog([], _, Acc) ->
    Acc.

uri_pattern(Method, Prefix, UriPath) ->
    Str = "^" ++ atom_to_list(Method) ++ ":" ++ Prefix ++
	re:replace(UriPath, "{([a-z_]+)}",
		   "(?<\\g{1}>[^/,? ]+)",
		   [global, {return, list}]) ++ "$",
    re:compile(Str).

%%--------------------------------------------------------------------
%% @doc URI Path로 라우팅할 Handler를 검색한다.
%% @spec (Sbj, RegExPatterns) -> Handler | undefined.
%% Sbj = string()
%% RegExPatterns = List
%% @end
%%--------------------------------------------------------------------
find_handler(_, []) ->
    undefined;
find_handler(Sbj, [R=#route{pattern=Pt}|T]) ->
    case re:run(Sbj, Pt, [{capture, all_names, binary}]) of
	{match, L} ->
	    {namelist, N} = re:inspect(Pt, namelist),
	    %% Path Parameter : /path/to/user/{user_id}
	    %% user_id=Value
	    NameMap = [{binary_to_list(Name), binary_to_list(Value)}
		       || {Name, Value} <- lists:zip(N, L)],
	    {R#route.auth, R#route.module, R#route.handler, NameMap};
	match ->
	    {R#route.auth, R#route.module, R#route.handler, []};
	nomatch ->
	    find_handler(Sbj, T)
    end.
