-module(api_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 1, 5},
	   [{api_router,
	     {api_router, start_link, []},
	     permanent,
	     5000,
	     worker,
	     [api_router]
	    }]
	 }}.
