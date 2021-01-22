-module(mod_api).

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

%%--------------------------------------------------------------------
%% Inets httpd api callback
%% OPTION, HEAD 는 지원하지 않음.
%%--------------------------------------------------------------------
do(Mod) ->
    Ocurr = string:str(Mod#mod.request_uri,
		       binary_to_list(api_router:get_prefix())),
    if
	Ocurr == 0 ->
	    response(404, [{result, error},
			   {reason, <<"Not Found">>}]);
	true ->
	    check_content_type(Mod)
    end.

%%--------------------------------------------------------------------
%% @doc
%% check whether "content-type" header is "application/json"
%% @spec
%% @end
%%--------------------------------------------------------------------
check_content_type(Mod) ->
    case proplists:get_value("content-type", Mod#mod.parsed_header) of
	"application/json" ->
	    UriMap = uri_string:parse(Mod#mod.request_uri),
	    Method = Mod#mod.method,
	    Header = Mod#mod.parsed_header,
	    Params = parse_param(Method, UriMap,
				 Mod#mod.entity_body),
	    {HttpCode, Response} = route_to_handler(maps:get(path, UriMap),
						    Method, Header, Params),
	    response(HttpCode, Response);
	_ ->
	    response(400, [{result, error},
			   {reason, <<"Bad Request">>}])
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
		misclib:json_to_terms(Body)
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
	nomatch ->
	    {403, [{fail, <<"Requested endpoint is forbidden">>}]};
	{Module, Func, PathParams, UserInfo} ->
	    Model = api_model:start([PathParams ++ Params, Header, UserInfo]),
	    logger:debug("path param:~p", [Model(param, [])]),
	    try
		apply(binary_to_atom(Module), binary_to_atom(Func), [Model]),
		%% response 호출과 함께 Model server의 Timeout 시작
		{200, {Model(mode, []), Model(purge, [])}}
	    catch
		%% handler 에서 throw 한 경우로 정상코드(200) 처리.
		%% -> {code, reason} 으로 변경
		throw:{Code, Reason} ->
		    Model(clear, []),
		    Model(put, {reason, Reason}),
		    {Code, Model(purge, [])};
		%% 시스템에서 발생한 오류로 서버츠 에러(500) 처리.
		error:Err:Stack ->
		    logger:error("handler error:~p~n", [Err]),
		    logger:error("handler stack:~p~n", [Stack]),
		    Model(clear, []),
		    Model(put, {result, error}),
		    Model(put, {reason, Err}),
		    {500, Model(purge, [])}
		    %% after
		    %% 	logger:debug("try after...")
		    %% 	Model(stop, [])
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
response(HttpCode, Output) ->
    IoList = case Output of
		 {raw, Payload} ->
		     lists:flatten(io_lib:format("~p", [Payload]));
		 {json, Payload} ->
		     misclib:terms_to_json(Payload);
		 Payload ->
		     misclib:terms_to_json(Payload)
	     end,
    Hd = [{code, HttpCode},
	  {content_type, "application/json"},
	  %%{'Access-Control-Allow-Origin', "*"},
	  {content_length, integer_to_list(iolist_size(IoList))}],
    {break, [{response, {response, Hd, [IoList]}}]}.
