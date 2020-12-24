%%%-------------------------------------------------------------------
%%% @author Dosung Noh <noh@iMac27.local>
%%% @copyright (C) 2020, Dosung Noh
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2020 by Dosung Noh <noh@iMac27.local>
%%%-------------------------------------------------------------------
-module(es).

-export([request/2]).

-define(HOST, "http://localhost:9200").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Body 데이터가 사용되므로 post 방식이어야 함.
%% @end
%%--------------------------------------------------------------------
request(Url, Query) ->
    {Ok, Response} =
	httpc:request(post,
		      {?HOST ++ Url, [], "application/json", Query},
		      [], []),
    if Ok =:= ok ->
	    {_Status, _Header, Body} = Response,
	    logger:debug("es response:~p~n", [Body]),
	    {ok, Body};
       true ->
	    logger:debug("es error=>"),
	    logger:error("es request error:~p~n", [Response]),
	    {error, Response}
    end.

%% decode(Body) ->
%%     {PropList} = jiffy:decode(Body),
%%     case hd(PropList) of
%% 	{<<"error">>, _} ->
%% 	    logger:error("es encode error:~p~n", [PropList]),
%% 	    {error, PropList};
%% 	_ ->
%% 	    {ok, PropList}
%%     end.
