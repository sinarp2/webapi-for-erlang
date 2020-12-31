-module(handler_user).

-export([users/1, user/1]).

-include("macros.hrl").

user(Model) ->
    UserId = Model(param, "user_id"),
    if
	UserId == undefined ->
	    throw(insufficient_path_info);
	true ->
	    ok
    end,
    logger:debug("User Id : ~p~n", [UserId]),

    {ok, Result} = es:search_by_id(<<"users">>, <<"_doc">>, list_to_binary(UserId)),
    ResultData = facade:to_ejson(Result),
    Source = ?prop(<<"_source">>, ResultData),
    Model(put, {result, success}),
    Model(put, {username, ?prop(<<"first_name">>, Source)}),
    Model(put, {lastname, ?prop(<<"last_name">>, Source)}),
    Model(put, {email, ?prop(<<"first_name">>, Source)}).

users(Model) ->
    %% Parameter Validation 기능???
    %% routes.config에 필수 파라미터 정보를 기입할 것인가???

    Size = Model(param, ["_size", int, 5]),
    From = Model(param, ["_page", int, 0]),

    Query = [{fields, [<<"first_name">>,
		       <<"last_name">>,
		       <<"email">>]},
	     {<<"_source">>, false},
	     {query,
	      [{match_all, [{}]}]},
	     {size, Size},
	     {from, From}],

    case es:search(<<"users">>, Query) of
	{ok, Resp} ->
	    %% es에서는 json 문자열로 리턴됨 "{\"took\":1,\"timed_out\":false
	    %% io_list로 리턴해야되기 때문에 binary로 변환해야함
	    %% <<"{\"took\":1,\"timed_out\":fals...>>
	    %% 문자열이 아닌 json 객체로 리턴하려면 jiffy:decode !!! 해야함.
	    %% string은 integer list로 표현되므로
	    %% binary (<<"string type">>) 형태로 변환해 주어야 함.
	    %% => string을 그대로 jiffy encode하게 되면 integer list
	    %% 로 표시됨 [12, 234, 432, 12, 332, ...] 형태
	    %% binary형태로 변환하면 json string으로 변환됨
	    %% javascript에서 인식된 json object로 리턴하려면
	    %% json 문자열을 encding

	    %% 데이터 형식은 상황에 따라 달라지므로
	    %% elasticsearch 사용 json 문자열을 대상으로한
	    %% 예만 제시.
	    Sample = "io_list로 리턴해야되기 때문에 binary로 변환해야함",

	    Model(put, {sample1, unicode:characters_to_binary(Sample)}),
	    Model(put, {sample2, Sample}),
	    %%Model(put, Sample),
	    Model(put, {result_string, unicode:characters_to_binary(Resp)}),
	    Model(put, {result, facade:to_ejson(Resp)});
	{error, Resp} ->
	    Model(put, {result, facade:to_ejson(Resp)})
    end.
