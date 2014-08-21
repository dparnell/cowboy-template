%% @private
-module(template_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    RestartStrategy = { one_for_one, 10, 10 },

    SessionSpecs = [
                    {template_session, {template_session, start_link, []}, permanent, 1000, worker, [template_session]}
                   ],

    Children = lists:append([SessionSpecs]),

    {ok, {RestartStrategy, Children}}.
