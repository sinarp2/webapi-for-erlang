-module(misclib).

-export([json_to_terms/1,
	 terms_to_json/1]).

-export([token_encode/4,
	 token_decode/2]).


json_to_terms(JsonData) when is_list(JsonData) ->
    jsx:decode(list_to_binary(JsonData), []);
json_to_terms(JsonData) ->
    %% jiffy는문자열을 받지만 jsx는 바이너리로 받음.
    %% jiffy:decode(JsonStr).
    jsx:decode(JsonData, []).

terms_to_json(Data) ->
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
    case proplists:get_value("authorization", Header) of
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
