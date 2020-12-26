-module(router_model).
-behavior(gen_server).

-export([start/1, stop/2, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).

-export([param/2, header/2, authinfo/2, put/2, get/2]).

-include("macros.hrl").

-record(state, {params, header, authinfo, data={[]}}).

stop(Pid, _Arg) ->
    gen_server:cast(Pid, stop).

param(Pid, []) ->
    gen_server:call(Pid, param);
param(Pid, Name) ->
    gen_server:call(Pid, {param, Name}).

%% 헤더 명은 모두 소문자로 저장이 되므로
%% 검색 시 소문자로 검색해야 함.
header(Pid, []) ->
    gen_server:call(Pid, header);
header(Pid, Name) ->
    gen_server:call(Pid, {header, Name}).

authinfo(Pid, []) ->
    gen_server:call(Pid, authinfo);
authinfo(Pid, Name) ->
    gen_server:call(Pid, {authinfo, Name}).

put(Pid, {Name, Value}) ->
    gen_server:call(Pid, {put, {Name, Value}}).

get(Pid, []) ->
    gen_server:call(Pid, get);
get(Pid, Name) ->
    get_server:call(Pid, {get, Name}).

start(InitArgs) ->
    {ok, Pid} = gen_server:start_link(?MODULE, InitArgs, []),
    %% FunAtom : param, header, authinfo, store
    fun(FunAtom, Args) ->
	    logger:debug("Request Model: ~p ~p~n", [Pid, FunAtom]),
	    case apply(router_model, FunAtom, [Pid, Args]) of
		{fail, Reason} ->
		    error(Reason);
		Value ->
		    Value
	    end
    end.

init([Params, Header, AuthData]) ->
    process_flag(trap_exit, true),
    {ok, #state{params=Params,
		header=Header,
		authinfo=AuthData}}.

handle_call(param, _, State) ->
    {reply, State#state.params, State};
handle_call({param, Name}, _, State) ->
    Value = ?prop(Name, State#state.params),
    {reply, Value, State};
handle_call(header, _, State) ->
    {reply, State#state.header, State};
handle_call({header, Name}, _, State) ->
    Value = ?prop(Name, State#state.header),
    {reply, Value, State};
handle_call(authinfo, _, State) ->
    {reply, State#state.authinfo, State};
handle_call({authinfo, Name}, _, State) ->
    Value = ?prop(Name, State#state.authinfo),
    {reply, Value, State};
handle_call(get, _, State) ->
    {reply, State#state.data, State};
handle_call({get, Name}, _, State) ->
    Value = ?prop(Name, State#state.data),
    {reply, Value, State};
handle_call({put, NewData}, _, State) ->
    {List} = State#state.data,
    NewList = List ++ [NewData],
    {reply, NewData, State#state{data={NewList}}};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(Reason, _) ->
    logger:debug("router_model terminates: ~p~n", [Reason]),
    ok.
