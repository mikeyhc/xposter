%%%-------------------------------------------------------------------
%% @doc xposter public API
%% @end
%%%-------------------------------------------------------------------

-module(xposter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Token = case os:getenv("DISCORD_TOKEN") of
                false -> throw({missing_env, "DISCORD_TOKEN"});
                V -> V
            end,
    Msgs = #{},
    discordant:connect(Token),
    discordant:set_routes(Msgs, []),
    xposter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
