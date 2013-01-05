atdc
====

Automation tool for downloading content

=== Usage ===<br>
erl -pa ebin<br>
mnesia:start().<br>
ssl:start().<br>
inets:start().<br>
rss:init().<br>
rss:add(FeedName, HtmlLink).<br>
rss:update_feed(FeedName).<br>
