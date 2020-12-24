-module(router_model).
-behavior(gen_server).

-export([start/1, stop/1, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).

-export([param/2, header/2, authinfo/2, put/2, get/2]).

-include("macros.hrl").

-record(state, {params, header, authinfo, data={[]}}).

param(Pid, Name) ->
    gen_server:call(Pid, {param, Name}).

%% 헤더 명은 모두 소문자로 저장이 되므로
%% 검색 시 소문자로 검색해야 함.
header(Pid, Name) ->
    gen_server:call(Pid, {header, Name}).

authinfo(Pid, all) ->
    gen_server:call(Pid, {authinfo, all});
authinfo(Pid, Name) ->
    gen_server:call(Pid, {authinfo, Name}).

put(Pid, {Name, Value}) ->
    gen_server:cast(Pid, {put, {Name, Value}}).

get(Pid, all) ->
    gen_server:call(Pid, {get, all});
get(Pid, Name) ->
    get_server:call(Pid, {get, Name}).

start(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([Params, Header, AuthData]) ->
    process_flag(trap_exit, true),
    {ok, #state{params=Params,
		header=Header,
		authinfo=AuthData}}.

handle_call({param, all}, _From, State) ->
    {reply, State#state.params, State};
handle_call({param, Name}, _From, State) ->
    Value = ?prop(Name, State#state.params),
    {reply, Value, State};
handle_call({header, all}, _From, State) ->
    {reply, State#state.header, State};
handle_call({header, Name}, _From, State) ->
    logger:debug("Get Header: ~p~n~p~n", [Name, State#state.header]),
    Value = ?prop(Name, State#state.header),
    {reply, Value, State};
handle_call({get, all}, _From, State) ->
    {reply, State#state.data, State};
handle_call({get, Name}, _From, State) ->
    Value = ?prop(Name, State#state.data),
    {reply, Value, State};
handle_call({authinfo, all}, _From, State) ->
    {reply, State#state.authinfo, State};
handle_call({authinfo, Name}, _From, State) ->
    Value = ?prop(Name, State#state.authinfo),
    {reply, Value, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({put, NewData}, State) ->
    {List} = State#state.data,
    NewList = List ++ [NewData],
    {noreply, State#state{data={NewList}}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:debug("ModelProxy terminates...", []),
    ok.
