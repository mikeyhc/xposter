-module(discord_guild).
-include_lib("kernel/include/logger.hrl").

-export([install/0]).
-export([add_guild/2, add_channel/2, remove_guild/1, remove_channel/2,
         get_guilds/0, get_guild/1]).

-record(discord_guild, {id :: binary(),
                        name :: binary(),
                        channels=[] :: [binary()]}).

install() ->
    case lists:member(discord_guild, mnesia:system_info(tables)) of
        true ->
            ?LOG_INFO("discord_guild table already installed"),
            ok;
        false ->
            ?LOG_INFO("installing discord_guild table"),
            mnesia:create_table(discord_guild,
                                [{attributes,
                                  record_info(fields, discord_guild)},
                                 {disc_copies, [node()]}])
    end.

add_guild(Id, Name) ->
    Entry = #discord_guild{id=Id, name=Name},
    F = fun() ->
                case mnesia:read({discord_guild, Id}) of
                    [] -> mnesia:write(Entry);
                    _ -> {error, <<"guild exists">>}
                end
        end,
    mnesia:activity(transaction, F).

add_channel(Guild, Channel) ->
    F = fun() ->
                case mnesia:read({discord_guild, Guild}) of
                    [] -> {error, <<"no such guild">>};
                    [_,_|_] -> {error, <<"multiple matching guilds">>};
                    [G=#discord_guild{channels=C}] ->
                        case lists:member(Channel, C) of
                            true -> ok;
                            false ->
                                Entry = G#discord_guild{channels=[Channel|C]},
                                mnesia:write(Entry)
                        end
                end
        end,
    mnesia:activity(transaction, F).

remove_guild(Guild) ->
    F = fun() -> mnesia:delete({discord_guild, Guild}) end,
    mnesia:activity(transaction, F).

remove_channel(Guild, Channel) ->
     F = fun() ->
                case mnesia:read({discord_guild, Guild}) of
                    [] -> {error, <<"no such guild">>};
                    [_,_|_] -> {error, <<"multiple matching guilds">>};
                    [G=#discord_guild{channels=C}] ->
                        NC = lists:delete(Channel, C),
                        Entry = G#discord_guild{channels=NC},
                        mnesia:write(Entry)
                end
         end,
     mnesia:activity(transaction, F).

get_guilds() ->
    Fold = fun(X, Acc) -> [guild_to_dict(X)|Acc] end,
    F = fun() -> mnesia:foldl(Fold, [], discord_guild) end,
    mnesia:activity(transaction, F).

get_guild(Guild) ->
     F = fun() ->
                case mnesia:read({discord_guild, Guild}) of
                    [] -> false;
                    [_,_|_] -> {error, <<"multiple matching guilds">>};
                    [G] -> guild_to_dict(G)
                end
         end,
     mnesia:activity(transaction, F).

guild_to_dict(#discord_guild{name=Name, id=Id, channels=Channels}) ->
    #{<<"name">> => Name, <<"id">> => Id, <<"channels">> => Channels}.
