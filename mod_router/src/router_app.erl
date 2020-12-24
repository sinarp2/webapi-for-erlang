-module(router_app).
-behavior(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(router_app).

start(_StartType, _StartData) ->
    router_sup:start_link().

stop(_Data) ->
    ok.
