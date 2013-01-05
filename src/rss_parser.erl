-module(rss_parser).
-include_lib("xmerl/include/xmerl.hrl").
-include("rss.hrl").
-export([file/1,
		 string/1]).

file(File) ->
	{Doc, _Rest} = xmerl_scan:file(File),
	xml(Doc).
string(String) ->
	{Doc, _Rest} = xmerl_scan:string(String),
	xml(Doc).
xml(Doc) ->
	XmlItems = xmerl_xpath:string("//item[link]", Doc),
	lists:map(
		fun(XmlItem) ->
			Title = get_field(XmlItem, "title"),
			Desc = get_field(XmlItem, "description"),
			Link = get_field(XmlItem, "link"),
			Guid = get_field(XmlItem, "guid"),
			PudDate = get_field(XmlItem, "pudDate"),
			#rss_item{title=Title,
	 				  desc=Desc,
		 			  link=Link,
			 		  guid=Guid,
			 		  pud_date=PudDate}
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