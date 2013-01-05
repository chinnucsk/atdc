atdc
====

Automation tool for downloading content

=== Usage ===
erl -pa ebin
mnesia:start().
ssl:start().
inets:start().
rss:init().
rss:add(FeedName, HtmlLink).
rss:update_feed(FeedName).
