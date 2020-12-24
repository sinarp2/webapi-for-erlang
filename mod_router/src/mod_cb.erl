-module(mod_cb).

-export([do/1]).
-export([]).

-include_lib("inets/include/httpd.hrl").
-include("macros.hrl").

%%--------------------------------------------------------------------
%% Inets httpd api callback
%% OPTION, HEAD 는 지원하지 않음.
%%--------------------------------------------------------------------
do(Mod) ->
    logger:debug("callback called : ~p~n", [self()]),
    case ?prop("content-type", Mod#mod.parsed_header) of
	"multipart/form-data" ->
	    response({400, {[{fail, <<"multipart/form-data not implemented.">>}]}});
	"application/json" ->
	    UriMap = uri_string:parse(Mod#mod.request_uri),
	    Method = Mod#mod.method,
	    Header = Mod#mod.parsed_header,
	    Params = parse_param(Method, UriMap,
				 Mod#mod.entity_body),
	    Result = route_to_handler(maps:get(path, UriMap),
				      Method, Header, Params),
	    response(Result);
	undefined ->
	    response({400, {[{fail, <<"content-type header not found.">>}]}});
	_ ->
	    response({400, {[{fail, <<"not supported content-type.">>}]}})
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
parse_param(Method, UriMap, _Body) when
      Method =:= "GET";
      Method =:= "DELETE"->
    uri_string:dissect_query(maps:get(query, UriMap, ""));
parse_param(_Method, _UriMap, Body) ->
    {BodyList} =
	case length(Body) of
	    0 ->
		{[]};
	    _ ->
		jiffy:decode(Body)
	end,
    [{binary_to_list(Name), Value} ||
	{Name, Value} <- BodyList].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
route_to_handler(Path, Method, Header, Params) ->
    MethodAtom = list_to_atom(string:lowercase(Method)),
    case router_srv:get_handler(Path, MethodAtom, Header) of
	%% auth check not implemented
	[_, {unauthorized, _Reason}] ->
	    {401, {[{fail, <<"unauthorized">>}]}};
	not_found ->
	    {404, {[{fail, <<"page not found">>}]}};
	[Handler, AuthInfo] ->
	    %% {handler_module, func, [uri parameter]}
	    %% ex: {handler_user,user,[{"user_id","FwfNrXMBVPiqh5flaLV1"}]}
	    %% authentication 정보 추가 되어야 함.
	    logger:debug("Handler: ~p~n", [Handler]),
	    case check_auth(AuthInfo) of
		{ok, AuthData} ->
		    process_logic(Handler, Header, Params, AuthData);
		Failed ->
		    logger:debug("Authentication failed for : ~p~n", [Failed]),
		    {401, {[{fail, <<"unauthorized.">>}]}}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_auth({Type, Data}) ->
    logger:debug("Authentication Type is : ~p~n", [Type]),
    logger:debug("Authentication Data is : ~p~n", [Data]),
    {ok, []}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
process_logic({Mod, Fun, PathParams}, Header, Params, AuthData) ->
    logger:debug("Processing request : ~p ~p ~p ~p~n",
		 [Mod, Fun, Header, Params ++ PathParams]),
    {ok, ThePid} = router_model:start([Params ++ PathParams,
				       Header, AuthData]),
    %% FunAtom : param, header, authinfo, store
    ModelProxy =
	fun(FunAtom, Name) ->
		logger:debug("Request Model Pid : ~p~n", [ThePid]),
		case apply(router_model, FunAtom, [ThePid, Name]) of
		    {fail, Reason} ->
			error(Reason);
		    Value ->
			Value
		end
	end,
    logger:debug("All Headers : ~p~n", [ModelProxy(header, all)]),
    logger:debug("Header : ~p~n", [ModelProxy(header, "myheader")]),
    try
	Model = apply(Mod, Fun, [ModelProxy]),
	logger:debug("model get:~p~n", [Model(get, all)]),
	{200, Model(get, all)}
    catch
	error:Err:Stacktrace ->
	    logger:error("handler error:~p~n", [Err]),
	    logger:error("handler stack:~p~n", [Stacktrace]),
	    {500, {[{<<"fail">>, <<"Handler Failure">>}]}}
    after
	router_model:stop(ThePid)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
response({Code, Payload}) ->
    IoList = jiffy:encode(Payload),
    Hd = [{code, Code},
	  {content_type, "application/json"},
	  %%{'Access-Control-Allow-Origin', "*"},
	  {content_length, integer_to_list(iolist_size(IoList))}],
    {break, [{response, {response, Hd, [IoList]}}]}.
