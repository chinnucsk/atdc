-module(rss).
-include("rss.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([init/0,
         add_rss/2,
         update_rss_feed/1]).

init() ->
    %% create table for rss
    mnesia:create_table(rss, 
                        [{ram_copies, [node()]},
                         {record_name, rss},
                         {attributes, record_info(fields, rss)},
                         {type, set}]).

add_rss(RssName, HtmlLink)
  when is_atom(RssName), is_list(HtmlLink) ->
    case catch mnesia:create_table(rss_feed,
                                   [{ram_copies, [node()]},
                                    {record_name, rss_feed},
                                    {attributes, record_info(fields, rss_feed)},
                                    {type, set}]) of
            {'EXIT', _} ->
                %% Already exists
                ok;
            _ ->
                ok
    end,
    %% Add feed to mnesia
    RssFeed = #rss_feed{name=RssName,
                        html_link=HtmlLink},
     F = fun() ->
            mnesia:write(rss_feed, RssFeed, write)
         end,
    mnesia:transaction(F).
    

update_rss_feed(RssName) 
  when is_atom(RssName) ->
    Q = qlc:q( 
          [E || E <- mnesia:table(rss_feed),
                     E#rss_feed.name == RssName]
     ),
    F = fun() -> qlc:e(Q) end,
    {atomic, RssFeeds} = mnesia:transaction(F),
    io:format("RssFeeds:~p~n", [RssFeeds]),
    lists:foreach(
        fun(#rss_feed{name=Name, html_link=HtmlLink}) ->
            io:format("Updating Rss:~p~n", [Name]),
            %% Retreive rss, parse and update mnesia
            {ok, {_, _, Xml}} = httpc:request(get, {HtmlLink, []}, [], []),
            RssItems = rss_parser:string(Xml),
            lists:foreach(
                fun(RssItem) ->
                        Fun = fun() ->
                            mnesia:write(rss, #rss{feed_name=RssName,
                                                   item=RssItem})
                        end,
                        mnesia:transaction(Fun)
                end,
                RssItems)
        end,
        RssFeeds).