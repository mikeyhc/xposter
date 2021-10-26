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
    discord_guild:install(),
    discordant:connect(Token),
    Msgs = #{
      <<"start">> => #{call => {xposter, add_channel, []}},
      <<"stop">> => #{call => {xposter, remove_channel, []}}
     },
    discordant:set_routes(Msgs, []),
    Hooks = #{
      <<"GUILD_CREATE">> => {xposter, guild_create, []},
      <<"MESSAGE_CREATE">> => {xposter, message_create, []}
     },
    discordant:set_hooks(Hooks),
    xposter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
