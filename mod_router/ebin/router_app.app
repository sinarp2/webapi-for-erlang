{application, router_app,
 [{description, "INETS API ROUTER"},
  {vsn, "1.0.0"},
  {modules, [router_app, router_sup, router_srv]},
  {registered, [router_sup, router_srv]},
  {applications, []},
  {mod, {router_app, []}}
 ]}.
