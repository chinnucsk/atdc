-record(rss_item, {
	title :: string(),
	desc  :: string(),
	link  :: string(),
	guid  :: string(),
	pud_date :: string()
}).

-record(rss, {
	feed_name :: atom(),
	item :: #rss_item{}
}).

-record(rss_feed, {
	name      :: atom(),
	html_link :: string()
}).