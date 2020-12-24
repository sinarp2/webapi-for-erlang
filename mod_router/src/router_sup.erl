-module(router_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, router_sup}, router_sup, []).

init([]) ->
    {ok, { {one_for_one, 1, 5},
	   [{router_srv,
	     {router_srv, start_link, []},
	     permanent,
	     5000,
	     worker,
	     [router_srv]
	    }]
	 }}.
