-module(router_srv).
-behavior(gen_server).

-export([start_link/0,
	 init/1,
	 handle_call/3]).

-export([get_handler/3]).

-record(state, {prefix,
		routes,
		authentication}).

-record(route, {pattern,
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
    RouteData = gen_server:call(?MODULE, get_routes),
    %% Method는 "POST", "GET" 문자열이고 Router 설정에는
    %% get, post 등 atom형대로 정의 되었음.
    case find_handler(atom_to_list(Method) ++ ":" ++ Path,
		      RouteData#state.routes) of
	not_found ->
	    not_found;
	Handler ->
	    [Handler, RouteData#state.authentication]
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------
%% 라우터 설정 파일을 읽어 라우터 내용을
%% 상태데이터로 설정한다.
%%-----------------------------------------
init([]) ->
    {ok, Path} = application:get_env(routes),
    logger:debug("Routes path : ~p~n", [Path]),
    {ok, [InfoList]} = file:consult(Path),
    %%logger:debug("Route Info: ~p~n", [InfoList]),
    Prefix = ?prop(prefix, InfoList),
    Routes = ?prop(routes, InfoList),
    Auth = ?prop(authentication, InfoList),
    RouteIndex = build_index(Prefix, Routes, [], []),
    %%logger:debug("RouteIndex: ~p~n", [RouteIndex]),
    {ok, #state{prefix=Prefix,
		routes=RouteIndex,
		authentication=Auth}}.

handle_call(get_routes, _From, RouteData) ->
    {reply, RouteData, RouteData}.

%%--------------------------------------------------------------------
%% @doc RESTful URI Path로 Handler 검색을 위한 RegEx 생성
%% @spec
%% @end
%%--------------------------------------------------------------------
build_index(_, [], _, Acc) ->
    Acc;
build_index(C, [H|T], E, Acc) ->
    build_index(C, T, E, build_index(C, H, E, Acc));
build_index(C, {X, Y}, E, Acc) ->
    build_index(C, Y, [X|E], Acc);
build_index(C, {M, H, D}, E, Acc) ->
    R = #route{path=lists:nth(1, E), method=M,
	       module=lists:nth(2, E), handler=H, description=D},
    Str = "^" ++ atom_to_list(M) ++ ":" ++ C ++
	re:replace(R#route.path,
		   "{([a-z_]+)}", "(?<\\g{1}>[^/,? ]+)",
		   [global, {return, list}]) ++ "$",
    {ok, Pt} = re:compile(Str),
    [R#route{pattern=Pt}|Acc].

%%--------------------------------------------------------------------
%% @doc URI Path로 라우팅할 Handler를 검색한다.
%% @spec (Sbj, RegExPatterns) -> Handler | not_found.
%% Sbj = string()
%% RegExPatterns = List
%% @end
%%--------------------------------------------------------------------
find_handler(_, []) ->
    not_found;
find_handler(Sbj, [R=#route{pattern=Pt}|T]) ->
    case re:run(Sbj, Pt, [{capture, all_names, binary}]) of
	nomatch ->
	    find_handler(Sbj, T);
	match ->
	    {R#route.module, R#route.handler, []};
	{match, L} ->
	    {namelist, N} = re:inspect(Pt, namelist),
	    NameMap = [{binary_to_list(Name), binary_to_list(Value)}
		       || {Name, Value} <- lists:zip(N, L)],
	    {R#route.module, R#route.handler, NameMap}
    end.
