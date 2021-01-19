%%%-------------------------------------------------------------------
%%% @author Dosung Noh <noh@iMac27.local>
%%% @copyright (C) 2020, Dosung Noh
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2020 by Dosung Noh <noh@iMac27.local>
%%%-------------------------------------------------------------------
-module(es).

-export([search/2, search_by_id/3, insert/3]).

-define(HOST, <<"http://localhost:9200/">>).
-define(CTYPE, "application/json").

-include("macros.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Body 데이터가 사용되므로 post 방식이어야 함.
%% @end
%%--------------------------------------------------------------------
search(Index, Query) ->
    {ok, Body} = request_post([Index, <<"/_search">>], Query),
    %% es 결과 데이터에서 hits List를 추출
    Edata = misclib:to_ejson(Body),
    try
	?prop(<<"hits">>, ?prop(<<"hits">>, Edata))
    catch
	_:Reason:Stack ->
	    logger:error("es result error:~p", [Edata]),
	    logger:error("reason:~p", [Reason]),
	    logger:error("stack:~p", [Stack]),
	    error(wrong_es_data)
    end.

search_by_id(Index, Doc, Id) ->
    {ok, Body} = request_get([Index, <<"/">>, Doc, <<"/">>, Id]),
    misclib:to_ejson(Body).

insert(Index, Doc, Data) ->
    {ok, Res} = request_post([Index, <<"/">>, Doc], Data),
    ResData = misclib:to_ejson(Res),
    case proplists:get_value(<<"error">>, ResData) of
	undefined ->
	    ResData;
	Val ->
	    {fail, Val}
    end.

request_get(Uri) ->
    Url = iolist_to_binary([?HOST|Uri]),
    case httpc:request(get, {Url, [{"content-type", ?CTYPE}]}, [], []) of
	{ok, Result} ->
	    {_Status, _Header, Body} = Result,
	    {ok, Body};
	Err ->
	    error(Err)
    end.

request_post(Uri, Data) ->
    Url = iolist_to_binary([?HOST|Uri]),
    BodyData = misclib:stringify(Data),
    case httpc:request(post, {Url, [], ?CTYPE, BodyData}, [], []) of
	{ok, Result} ->
	    {_Status, _Header, Body} = Result,
	    {ok, Body};
	Err ->
	    error(Err)
    end.
