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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Body 데이터가 사용되므로 post 방식이어야 함.
%% @end
%%--------------------------------------------------------------------
search(Index, Query) ->
    request_post([Index, <<"/_search">>], Query).

search_by_id(Index, Doc, Id) ->
    request_get([Index, <<"/">>, Doc, <<"/">>, Id]).

insert(Index, Doc, Data) ->
    request_post([Index, <<"/">>, Doc], Data).

request_get(Uri) ->
    Url = iolist_to_binary([?HOST|Uri]),
    case httpc:request(get, {Url, [{"content-type", ?CTYPE}]}, [], []) of
	{ok, Result} ->
	    {_Status, _Header, Body} = Result,
	    {ok, Body};
	Err ->
	    Err
    end.

request_post(Uri, Data) ->
    Url = iolist_to_binary([?HOST|Uri]),
    BodyData = misclib:stringify(Data),
    case httpc:request(post, {Url, [], ?CTYPE, BodyData}, [], []) of
	{ok, Result} ->
	    {_Status, _Header, Body} = Result,
	    {ok, Body};
	Err ->
	    Err
    end.
