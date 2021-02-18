-module(mod_api).

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

-define(CONTENT_TYPE, "content-type").
-define(GET, "GET").
-define(DELETE, "DELETE").
-define(APPLICATION_JSON, "application/json").

%%--------------------------------------------------------------------
%% Inets httpd api callback
%% OPTION, HEAD 는 지원하지 않음.
%%--------------------------------------------------------------------
do(Mod) ->
    case string:str(Mod#mod.request_uri,
		    binary_to_list(api_router:get_prefix())) of
	0 ->
	    response(404, [{result, fail}, {reason, <<"Not Found">>}]);
	_ ->
	    Method = Mod#mod.method,
	    Header = Mod#mod.parsed_header,
	    UriMap = uri_string:parse(Mod#mod.request_uri),
	    Body = Mod#mod.entity_body,
	    CType = proplists:get_value(?CONTENT_TYPE, Header),
	    Params = request_parameter(Method, UriMap, CType, Body),
	    {HttpCode, Response} = route_to_handler(maps:get(path, UriMap),
						    Method, Header, Params),
	    response(HttpCode, Response)
    end.

%% GET, DELETE Method url parameter
%% application/json 타입은 request body로 json 형태의 파라미터를 전달하는데
%% GET, DELETE과는 Uri 파라미터 전달이다.
request_parameter(Method, UriMap, _, _)
  when Method =:= ?GET; Method =:= ?DELETE ->
    uri_string:dissect_query(maps:get(query, UriMap, ""));
%% application/json content type -> request body data
request_parameter(_, _, CType, Body)
  when CType =:= ?APPLICATION_JSON ->
    BodyList =
	case length(Body) of
	    0 ->
		[];
	    _ ->
		misclib:json_to_terms(Body)
	end,
    [{binary_to_list(Name), Value} ||
	{Name, Value} <- BodyList];
request_parameter(_, _, CType, Body) ->
    {IsMultipart, Boundary} = misclib:is_multipart(CType),
    if IsMultipart =:= true ->
	    misclib:parse_multipart_data(Boundary, Body);
       true -> nil
    end.

%%--------------------------------------------------------------------
%% @doc
%% check "content-type" header for parsing paramters
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
%% check_content_type(Mod) ->
%%     %% Axios Client 에서는 body행태가 아닌 param 형태의 URL이 사용되면
%%     %% content-type이 강제로 재설정 되어 application/json content type을 지정할 수 없다.
%%     %% GET 방식에서는 json 형태로 파라미터를 보낼 수 없으므로
%%     %% content-type이 application/json이 아니게 된다.
%%     case proplists:get_value("content-type", Mod#mod.parsed_header) of
%% 	"application/json" ->
%% 	    Method = Mod#mod.method,
%% 	    Header = Mod#mod.parsed_header,
%% 	    UriMap = uri_string:parse(Mod#mod.request_uri),
%% 	    Params = parse_param(Method, UriMap,
%% 				 Mod#mod.entity_body),
%% 	    {HttpCode, Response} = route_to_handler(maps:get(path, UriMap),
%% 						    Method, Header, Params),
%% 	    response(HttpCode, Response);
%% 	_ ->
%% 	    logger:debug("error:~p", [Mod#mod.parsed_header]),
%% 	    response(400, [{result, error},
%% 			   {reason, <<"content-type not found">>}])
%%     end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%% parse_param(Method, UriMap, _Body) when
%%       Method =:= "GET";
%%       Method =:= "DELETE"->
%%     uri_string:dissect_query(maps:get(query, UriMap, ""));
%% parse_param(_Method, _UriMap, Body) ->
%%     %% application/json인 경우만 해당함.
%%     logger:debug("post params:~p", [Body]),
%%     BodyList =
%% 	case length(Body) of
%% 	    0 ->
%% 		[];
%% 	    _ ->
%% 		misclib:json_to_terms(Body)
%% 	end,
%%     [{binary_to_list(Name), Value} ||
%% 	{Name, Value} <- BodyList].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
route_to_handler(_, _, _, Params) when Params =:= nil ->
    {400, [{result, fail}, {reason, <<"Invalid Request">>}]};
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
    %% logger:debug("~p", [Output]),
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
