-module(xposter).
-include_lib("kernel/include/logger.hrl").

-export([guild_create/2, add_channel/3, remove_channel/3, message_create/2]).

guild_create(_ApiPid, #{<<"d">> := Data}) ->
    #{<<"name">> := Name, <<"id">> := Id} = Data,
    ?LOG_INFO("adding guild ~s(~s)", [Name, Id]),
    discord_guild:add_guild(Id, Name).

add_channel([Type], _ApiPid,
            #{<<"guild_id">> := Gid, <<"channel_id">> := Cid}) ->
    ?LOG_INFO("adding channel ~s(~s) to ~s", [Gid, Cid, Type]),
    discord_guild:add_channel(Gid, Cid, Type),
    {reply, <<"monitoring started">>, []}.

remove_channel(_Args, _ApiPid,
               #{<<"guild_id">> := Gid, <<"channel_id">> := Cid}) ->
    ?LOG_INFO("removing channel ~s(~s)", [Gid, Cid]),
    discord_guild:remove_channel(Gid, Cid),
    {reply, <<"monitoring stopped">>, []}.

message_create(ApiPid, #{<<"d">> := Data}) ->
    case monitored(Data) and not user_self(Data) of
        true ->
            Type = channel_type(Data),
            case is_meme(Data) of
                false ->
                    case has_link(Data) of
                        false -> ok;
                        true -> repost_link(ApiPid, Type, Data)
                    end;
                true ->
                    repost_meme(ApiPid, Type, Data)
            end;
        false -> ok
    end.

monitored(#{<<"guild_id">> := Gid, <<"channel_id">> := Cid}) ->
    case discord_guild:get_guild(Gid) of
        false -> false;
        #{<<"channels">> := Channels} ->
            lists:keymember(Cid, 1, Channels)
    end.

channel_type(#{<<"guild_id">> := Gid, <<"channel_id">> := Cid}) ->
    case discord_guild:get_guild(Gid) of
        false -> false;
        #{<<"channels">> := Channels} ->
            {_, Type} = lists:keyfind(Cid, 1, Channels),
            Type
    end.

user_self(#{<<"author">> := #{<<"id">> := UID0}}) ->
    {ok, UID1} = discordant:user_id(),
    UID0 =:= UID1.

is_meme(Data) ->
    (maps:is_key(<<"attachments">>, Data) andalso
    length(maps:get(<<"attachments">>, Data)) > 0).

has_link(#{<<"content">> := Binary}) ->
    binary:match(Binary, <<"http">>) =/= nomatch.

repost_meme(ApiPid, Type,
            #{<<"guild_id">> := Gid, <<"attachments">> := Attach}) ->
    Guilds = lists:filter(fun(#{<<"id">> := Id}) -> Id =/= Gid end,
                           discord_guild:get_guilds()),
    F = fun(Guild) -> repost_meme(ApiPid, Type, Guild, Attach) end,
    lists:foreach(F, Guilds).

repost_meme(ApiPid, Type, Guild, Attachments) ->
    MapF = fun(#{<<"url">> := Url}) -> Url end,
    Embeds = lists:map(MapF, Attachments),
    Channels = valid_channels(Type, Guild),
    PostF = fun(C) -> fun(U) -> post_message(ApiPid, C, U) end end,
    FEF = fun(Channel) -> lists:foreach(PostF(Channel), Embeds) end,
    lists:foreach(FEF, Channels).

repost_link(ApiPid, Type, #{<<"guild_id">> := Gid, <<"content">> := Content}) ->
    Guilds = lists:filter(fun(#{<<"id">> := Id}) -> Id =/= Gid end,
                           discord_guild:get_guilds()),
    lists:foreach(fun(G) -> repost_link(ApiPid, Type, G, Content) end, Guilds).

repost_link(ApiPid, Type, Guild, Content) ->
    Channels = valid_channels(Type, Guild),
    FEF = fun(Channel) -> post_message(ApiPid, Channel, Content) end,
    lists:foreach(FEF, Channels).

post_message(ApiPid, Channel, Msg) ->
    ?LOG_INFO("posting to ~s: ~s", [Channel, Msg]),
    discord_api:send_message(ApiPid, Channel, Msg).

valid_channels(Type, #{<<"channels">> := Channels}) ->
    FM = fun({X, T}) when T =:= Type -> {true, X};
            (_) -> false
         end,
    lists:filtermap(FM, Channels).
