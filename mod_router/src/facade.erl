-module(facade).

-export([stringify/1, to_json/1]).
-export([token_encode/1, token_decode/1]).

-include("macros.hrl").

to_json(JsonStr) ->
    jiffy:decode(JsonStr).

stringify(Data) ->
    jiffy:encode(Data).

token_encode(Claims) ->
    {_Type, Config} = router_srv:get_authinfo(),
    Alg = ?prop(algorithm, Config),
logger:debug("alg:~p", [Alg]),
    Expiration = ?prop(expiration, Config),
    Key = ?prop(key, Config),
    jwt:encode(Alg, Claims, Expiration, Key).

token_decode(Header) ->
    case ?prop("authorization", Header) of
	undefined ->
	    {error, header_not_found};
	Value ->
	    token_decode(lexemes, Value)
    end.

token_decode(lexemes, HeaderValue) ->
    Token = string:lexemes(HeaderValue, " "),
    if length(Token) =/= 2 ->
	    {error, invalid_header};
       true ->
	    token_decode(decode, list_to_binary(tl(Token)))
    end;
token_decode(decode, Token) ->
    logger:debug("token:~p", [Token]),
    {_Type, Config} = router_srv:get_authinfo(),
    Key = ?prop(key, Config),
    case jwt:decode(Token, Key) of
	{ok, Info} ->
	    maps:to_list(Info);
	Error ->
	    Error
    end.
