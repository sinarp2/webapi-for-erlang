-module(misclib).

-export([json_to_terms/1,
	 terms_to_json/1]).

-export([token_encode/4,
	 token_decode/2]).

-export([is_multipart/1, parse_multipart_data/2]).

-define(MULTIPART_DESC_NAME, "name=\"(?<pname>\\S+)\"").
-define(MULTIPART_CONTENT_TYPE, "multipart/form-data\s?+;\s?+boundary=(?<boundary>\\S+)").

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

%%---------------------------------------------------------
%% Multipart Form Data
%%---------------------------------------------------------
is_multipart(ContentType) ->
    case re:run(ContentType, ?MULTIPART_CONTENT_TYPE,
		[{capture, all_names, list}]) of
	{match, [Boundary]} ->
	    {true, Boundary};
	_ -> {false, nil}
    end.

parse_multipart_data(Boundary, Data) ->
    BinaryData = iolist_to_binary(Data),
    Sep = boundary_separator(Boundary),
    Tem = boundary_terminator(Boundary),
    Parts = binary:split(BinaryData, [Sep, Tem], [global,trim_all]),
    multipart_parameters(Parts).

multipart_parameters([H|T]) ->
    Parts = binary:split(H, <<13,10,13,10>>, [global,trim_all]),
    Name = multipart_name(lists:nth(1, Parts)),
    ValuePart = lists:nth(length(Parts), Parts),
    Len = byte_size(ValuePart) - 2, %% remove trailing carage return and newline (\r\n)
    <<Value:Len/binary,_/binary>> = ValuePart,
    %%save(Value),
    [{Name,Value}|multipart_parameters(T)];
multipart_parameters([]) ->
    [].

multipart_name(Desc) ->
    case re:run(Desc, ?MULTIPART_DESC_NAME, [{capture, all_names, binary}]) of
	{_, [Name]} -> Name;
	_ -> <<"undefined">>
    end.

boundary_separator(Boundary) ->
    list_to_binary(lists:flatten(io_lib:format("--~s\r\n", [Boundary]))).

boundary_terminator(Boundary) ->
    list_to_binary(lists:flatten(io_lib:format("--~s--\r\n", [Boundary]))).
