-module(handler_main).

-export([index/1, apispec/1]).

-include("macros.hrl").

index(Model) ->
    Model(put, {result, success}).

apispec(Model) ->
    Routes = api_router:get_routes(),
    Model(mode, raw),
    Model(put, {routes, Routes}).
