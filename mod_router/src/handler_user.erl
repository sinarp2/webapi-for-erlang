-module(handler_user).

-export([users/1, user/1]).

user(Model) ->
    UserId = Model(param, "user_id"),
    logger:debug("User Id : ~p~n", [UserId]),
    {[{ok, <<"user data...">>}]}.

users(Model) ->
    %% Parameter Validation 기능???
    %% routes.config에 필수 파라미터 정보를 기입할 것인가???
    Size = case Model(param, "_size") of
	       undefined ->
		   5;
	       SizeStr ->
		   list_to_integer(SizeStr)
	   end,

    %% 1. not nil
    %% 2. can be converted to integer
    %% 3. _page > -1
    From = case Model(param, "_page") of
	       undefined ->
		   0;
	       Page ->
		   (list_to_integer(Page) - 1) * Size
	   end,

    logger:debug("parameter _page: ~p~n", [From]),
    logger:debug("parameter _size: ~p~n", [Size]),

    Chs = io_lib:format("{\"size\":~B, \"from\":~B}", [Size, From]),
    Query = lists:flatten(Chs),
    case es:request("/users/_search", Query) of
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
	    logger:debug("resp:~p~n", [Resp]),
	    Model(put, {result, jiffy:decode(Resp)});
	{error, Resp} ->
	    Model(put, {result, Resp})
    end,

    Model.
