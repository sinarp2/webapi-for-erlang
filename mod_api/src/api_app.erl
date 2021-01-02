-module(api_app).
-behavior(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(api_app).

start(_StartType, _StartData) ->
    api_sup:start_link().

stop(_Data) ->
    ok.
