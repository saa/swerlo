-module(swerlo_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    swerlo_sup:start_link().

stop(_State) ->
    ok.
