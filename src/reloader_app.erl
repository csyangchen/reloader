%% @private

-module(reloader_app).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init(_Args) ->
    {ok, {{one_for_one, 5, 10}, [
        {reloader, {reloader, start_link, []}, permanent, 5000, worker, [reloader]}
    ]}}.
