%% @private
-module(template_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    File = code:which(?MODULE),
    WWWDir = filename:join(filename:dirname(File), <<"../priv/www">>),

    Dispatch = cowboy_router:compile([
                {'_', [
                       %% app assets via /assets
                       {"/assets/[...]", cowboy_static, {dir, WWWDir}},
                       %% render the root page of the application for requests for /
                       {"/:template/[:id]", template_handler, []},
                       {"/[...]", template_handler, []}
                      ]}
               ]),

    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [
                                 {env, [{dispatch, Dispatch}] },
                                 {onrequest, fun(Req) -> template_session:new(Req) end },
                                 {onresponse, fun template_responder:respond/4}
                                ]
                               ),

    template_sup:start_link().

stop(_State) ->
    ok.
