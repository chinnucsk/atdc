atdc
====

Automation tool for downloading content

=== Usage ===
erl -pa ebin
mnesia:start().
ssl:start().
inets:start().
parser:create_table(rss_site_name).
%% If you have a link to the rss feed
Xml = parser:get_rss_feed(HttpLinkToRssFeed).
parser:parse_string(Xml).
%% If you have a xml file on disk
parser:parse_file(XmlFile).