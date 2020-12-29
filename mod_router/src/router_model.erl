-module(router_model).
-behavior(gen_server).

-export([start/1, stop/2, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).

-export([param/2, header/2, userinfo/2, put/2, get/2]).

-include("macros.hrl").

-record(state, {params, header, userinfo, session_data={[]}}).

stop(Pid, _Arg) ->
    gen_server:cast(Pid, stop).

param(Pid, []) ->
    %% 전체 파라미터 리턴
    gen_server:call(Pid, param);
param(Pid, [Name, Default]) ->
    %% 이름으로 파라미터 리턴
    %% 없으면 디폴트 값 리턴
    gen_server:call(Pid, {param, Name, Default});
param(Pid, [Name, Type, Default]) ->
    %% 이름으로 파라미터 리턴
    %% 파라미터 리턴 전 형 변환: int, float
    %% 없으면 디폴트 값 리턴
    gen_server:call(Pid, {param, Name, Type, Default});
param(Pid, Name) ->
    %% 이름으로 파라미터 리턴
    gen_server:call(Pid, {param, Name}).

%% 헤더 명은 모두 소문자로 저장이 되므로
%% 검색 시 소문자로 검색해야 함.
header(Pid, []) ->
    gen_server:call(Pid, header);
header(Pid, Name) ->
    gen_server:call(Pid, {header, Name}).

userinfo(Pid, []) ->
    gen_server:call(Pid, userinfo);
userinfo(Pid, Name) ->
    gen_server:call(Pid, {userinfo, Name}).

put(_Pid, Args) when not is_tuple(Args) ->
    erlang:error("Model put value must be a tuple");
put(Pid, {Name, Value}) ->
    NewValue =
	case io_lib:char_list(Value) of
	    true ->
		unicode:characters_to_binary(Value);
	    false ->
		Value
	end,
    gen_server:call(Pid, {put, {Name, NewValue}}).

get(Pid, []) ->
    gen_server:call(Pid, get);
get(Pid, Name) ->
    get_server:call(Pid, {get, Name}).

start(InitArgs) ->
    {ok, Pid} = gen_server:start_link(?MODULE, InitArgs, []),
    %% Cmd : param, header, userinfo, store
    fun(Cmd, Args) ->
	    case apply(?MODULE, Cmd, [Pid, Args]) of
		{fail, Reason} ->
		    error(Reason);
		Value ->
		    Value
	    end
    end.

init([Params, Header, UserInfo]) ->
    process_flag(trap_exit, true),
    {ok, #state{params=Params,
		header=Header,
		userinfo=UserInfo}}.

handle_call(param, _, State) ->
    {reply, State#state.params, State};
handle_call({param, Name}, _, State) ->
    Value = ?prop(Name, State#state.params),
    {reply, Value, State};
handle_call({param, Name, Default}, _, State) ->
    case ?prop(Name, State#state.params) of
	undefined ->
	    {reply, Default, State};
	Value ->
	    {reply, Value, State}
    end;
handle_call({param, Name, Type, Default}, _, State) ->
    case ?prop(Name, State#state.params) of
	undefined ->
	    {reply, Default, State};
	Value ->
	    {reply, convert(Type, Value), State}
    end;
handle_call(header, _, State) ->
    {reply, State#state.header, State};
handle_call({header, Name}, _, State) ->
    Value = ?prop(Name, State#state.header),
    {reply, Value, State};
handle_call(userinfo, _, State) ->
    {reply, State#state.userinfo, State};
handle_call({userinfo, Name}, _, State) ->
    Value = ?prop(Name, State#state.userinfo),
    {reply, Value, State};
handle_call(get, _, State) ->
    {reply, State#state.session_data, State};
handle_call({get, Name}, _, State) ->
    Value = ?prop(Name, State#state.session_data),
    {reply, Value, State};
handle_call({put, NewData}, _, State) ->
    {List} = State#state.session_data,
    NewList = List ++ [NewData],
    {reply, NewData, State#state{session_data={NewList}}};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    logger:debug("Info message:~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    logger:debug("Model Terminates:~p:~p", [Reason, self()]),
    ok.

convert(int, Value) ->
    list_to_integer(Value);
convert(float, Value) ->
    list_to_float(Value).
