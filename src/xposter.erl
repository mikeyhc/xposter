-module(xposter).
-include_lib("kernel/include/logger.hrl").

-export([guild_create/2, add_channel/3, remove_channel/3, message_create/2]).

guild_create(_ApiPid, #{<<"d">> := Data}) ->
    #{<<"name">> := Name, <<"id">> := Id} = Data,
    ?LOG_INFO("adding guild ~s(~s)", [Name, Id]),
    discord_guild:add_guild(Id, Name).

add_channel(_Args, _ApiPid,
            #{<<"guild_id">> := Gid, <<"channel_id">> := Cid}) ->
    ?LOG_INFO("adding channel ~s(~s)", [Gid, Cid]),
    discord_guild:add_channel(Gid, Cid),
    {reply, <<"monitoring started">>, []}.

remove_channel(_Args, _ApiPid,
               #{<<"guild_id">> := Gid, <<"channel_id">> := Cid}) ->
    ?LOG_INFO("removing channel ~s(~s)", [Gid, Cid]),
    discord_guild:remove_channel(Gid, Cid),
    {reply, <<"monitoring stopped">>, []}.

message_create(ApiPid, #{<<"d">> := Data}) ->
    case monitored(Data) andalso is_meme(Data) of
        false -> ok;
        true -> repost(ApiPid, Data)
    end.

monitored(#{<<"guild_id">> := Gid, <<"channel_id">> := Cid}) ->
    case discord_guild:get_guild(Gid) of
        false -> false;
        #{<<"channels">> := Channels} ->
            lists:member(Cid, Channels)
    end.

is_meme(Data) ->
    maps:is_key(<<"attachments">>, Data) andalso
    length(maps:get(<<"attachments">>, Data)) > 0.

repost(ApiPid, #{<<"guild_id">> := Gid, <<"attachments">> := Attach}) ->
    Guilds = lists:filter(fun(#{<<"id">> := Id}) -> Id =/= Gid end,
                           discord_guild:get_guilds()),
    F = fun(Guild) -> repost_attachments(ApiPid, Guild, Attach) end,
    lists:map(F, Guilds).

repost_attachments(ApiPid, Guild, Attachments) ->
    MapF = fun(#{<<"url">> := Url}) -> Url end,
    Embeds = lists:map(MapF, Attachments),
    #{<<"channels">> := Channels} = Guild,
    PostF = fun(C) -> fun(U) -> post_message(ApiPid, C, U) end end,
    FEF = fun(Channel) -> lists:foreach(PostF(Channel), Embeds) end,
    lists:foreach(FEF, Channels).

post_message(ApiPid, Channel, Url) ->
    ?LOG_INFO("posting to ~s: ~s", [Channel, Url]),
    discord_api:send_message(ApiPid, Channel, Url).
