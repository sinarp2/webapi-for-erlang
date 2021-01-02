-module(mod_api).

-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("macros.hrl").

%%--------------------------------------------------------------------
%% Inets httpd api callback
%% OPTION, HEAD 는 지원하지 않음.
%%--------------------------------------------------------------------
do(Mod) ->
    case ?prop("content-type", Mod#mod.parsed_header) of
	"multipart/form-data" ->
	    response({400,
		      [{result, error},
		       {reason, <<"multipart/form-data not implemented.">>}]});
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
	    response({400, [{result, error},
			    {reason, <<"content-type header not found.">>}]});
	_ ->
	    response({400, [{result, error},
			    {reason, <<"not supported content-type.">>}]})
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
    BodyList =
	case length(Body) of
	    0 ->
		[];
	    _ ->
		misclib:to_ejson(Body)
	end,
    [{binary_to_list(Name), Value} ||
	{Name, Value} <- BodyList].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
route_to_handler(Path, Method, Header, Params) ->
    case api_router:get_handler(Path, Method, Header) of
	%% auth check not implemented
	{unauthorized, Reason} ->
	    {401, [{fail, <<"unauthorized">>}, {reason, Reason}]};
	undefined ->
	    {404, [{fail, <<"handler not found">>}]};
	{Mod, Fun, PathParams, UserInfo} ->
	    process_logic(Mod, Fun, Header,
			  PathParams ++ Params, UserInfo)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
process_logic(Mod, Fun, Header, Params, UserInfo) ->
    Model = api_model:start([Params, Header, UserInfo]),
    try
	apply(Mod, Fun, [Model]),
	%% response 호출과 함께 Model server의 Timeout 시작
	{200, Model(get, [])}
    catch
	throw:Reason ->
	    Model(clear, []),
	    Model(put, {result, fail}),
	    Model(put, {reason, Reason}),
	    {200, Model(get, [])};
	error:Err:Stacktrace ->
	    logger:error("handler error:~p~n", [Err]),
	    logger:error("handler stack:~p~n", [Stacktrace]),
	    Model(clear, []),
	    Model(put, {result, error}),
	    Model(put, {reason, Err}),
	    {500, Model(get, [])}
    after
	logger:debug("try after...")
	%%Model(stop, [])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
response({Code, Payload}) ->
    {NewCode, IoList} = try_encode(Code, Payload),
    Hd = [{code, NewCode},
	  {content_type, "application/json"},
	  %%{'Access-Control-Allow-Origin', "*"},
	  {content_length, integer_to_list(iolist_size(IoList))}],
    {break, [{response, {response, Hd, [IoList]}}]}.

try_encode(Code, Payload) ->
    try
	IoList = misclib:stringify(Payload),
	{Code, IoList}
    catch
	Type:Reason:Stack ->
	    logger:error("encoding error at respose", []),
	    logger:error("~p:~p", [Type, Reason]),
	    logger:error("Stack:~p", [Stack]),
	    {500, [{result, error},
		   {reason, <<"Internal Server Error">>}]}
    end.
