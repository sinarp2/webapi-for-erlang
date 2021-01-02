{application, api_app,
 [{description, "WEB API WITH INETS"},
  {vsn, "1.0.0"},
  {modules, [api_app, api_sup, api_router]},
  {registered, [api_sup, api_router]},
  {applications, []},
  {mod, {api_app, []}}
 ]}.
