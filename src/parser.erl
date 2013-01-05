-module(parser).
-include_lib("xmerl/include/xmerl.hrl").
-export([parse_file/1,
		 parse_string/1,
		 create_table/1,
		 get_rss_feed/1]).

-record(rss_item, {
	title :: string(),
	desc  :: string(),
	link  :: string(),
	guid  :: string(),
	pud_date :: string()
	}).


get_rss_feed(Html) ->
	{ok, {_, _, Xml}} = httpc:request(get, {Html, []}, [], []),
	Xml.

create_table(Rss) ->
	mnesia:create_table(Rss, [{ram_copies, [node()]},
							  {record_name, rss_item},
							  {attributes, record_info(fields, rss_item)},
							  {type, set}]).

parse_file(File) ->
	{Doc, _Rest} = xmerl_scan:file(File),
	parse_xml(Doc).
parse_string(String) ->
	{Doc, _Rest} = xmerl_scan:string(String),
	parse_xml(Doc).
parse_xml(Doc) ->
	XmlItems = xmerl_xpath:string("//item[link]", Doc),
	lists:map(
		fun(XmlItem) ->
			Title = get_field(XmlItem, "title"),
			Desc = get_field(XmlItem, "description"),
			Link = get_field(XmlItem, "link"),
			Guid = get_field(XmlItem, "guid"),
			PudDate = get_field(XmlItem, "pudDate"),
			RssItem =
				#rss_item{title=Title,
		 				  desc=Desc,
			 			  link=Link,
				 		  guid=Guid,
				 		  pud_date=PudDate},
			F = fun() ->
					mnesia:write(scenehd, RssItem, write)
				end,
			mnesia:transaction(F)
		end,
		XmlItems).

get_field(XmlItem, FieldName) ->
	FieldVal = xmerl_xpath:string(FieldName ++ "[1]/text()", XmlItem),
	case FieldVal of
		[Val] ->
			Val#xmlText.value;
		[] ->
			[];
		Values -> %% In case xmerl has cut multiple lines into multiple items, put them together
			ListOfVal = [Val#xmlText.value || Val <- Values],
			lists:concat(ListOfVal)
	end.