-module(misclib).

-export([stringify/1,
	 to_ejson/1]).

-export([token_encode/4,
	 token_decode/2]).

-export([flatten/5]).

-include("macros.hrl").

to_ejson(JsonStr) ->
    %% jiffy는문자열을 받지만 jsx는 바이너리로 받음.
    %% jiffy:decode(JsonStr).
    jsx:decode(list_to_binary(JsonStr), []).

stringify(Data) ->
    %% jiffy는 {[]}로 시작
    %% {_} 이면 JSON Object
    %% [_] 이면 JSON Array
    %% jiffy:encode(Data).
    %% jsx는 []로 시작
    %% JSON Object -> [ {}, {}, .. ] -> 리스트 내에 튜플(K,V)만 존재함.
    %% JSON Array -> [ [{}, {}...], [{}, {}, ...] ] -> 리스트 내에 리스트들이 존재함.
    jsx:encode(Data).

token_encode(Claims, Alg, Expiration, Key) ->
    jwt:encode(Alg, Claims, Expiration, Key).

token_decode(Header, Key) ->
    case ?prop(?AUTH_HEADER, Header) of
	undefined ->
	    {error, header_not_found};
	HeaderVal ->
	    TokenList = string:lexemes(HeaderVal, " "),
	    if
		length(TokenList) =/= 2 ->
		    {error, invalid_header};
		true ->
		    Token = list_to_binary(tl(TokenList)),
		    case jwt:decode(Token, Key) of
			{ok, Info} ->
			    maps:to_list(Info);
			Err ->
			    Err
		    end
	    end
    end.

%% 계층 구조의 데이터를 테이블 구조로 변환
flatten(Nth, Roots, [H|T], Cols, Rcds) when length(Roots) >= Nth ->
    Next = lists:nth(Nth, Roots),
    NewCols = lists:append([Cols, proplists:delete(Next, H)]),
    NextGen = proplists:get_value(Next, H),
    flatten(Nth, Roots, T, Cols, flatten(Nth+1, Roots, NextGen, NewCols, Rcds));
flatten(Nth, Roots, [H|T], Cols, Rcds) when length(Roots) < Nth ->
    NewCols = lists:append([Cols, H]),
    flatten(1, Roots, T, Cols, [NewCols|Rcds]);
flatten(_, _, [], _, Rcds) ->
    Rcds.
