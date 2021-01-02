-module(misclib).

-export([stringify/1,
	 to_ejson/1]).

-export([token_encode/4,
	 token_decode/2]).

-include("macros.hrl").

to_ejson(JsonStr) ->
    %% jiffy는문자열을 받지만 jsx는 바이너리로 받음.
    %% jiffy:decode(JsonStr).
    jsx:decode(list_to_binary(JsonStr), []).

stringify(Data) ->
    %% jiffy는 {[]}로 시작
    %% jiffy:encode(Data).
    %% jsx는 []로 시작
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
