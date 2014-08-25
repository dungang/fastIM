%%%----------------------------------------------------------------------
%%% File    : odbc_queries.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : ODBC queries dependind on back-end
%%% Created : by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(odbc_queries).

-author("mremond@process-one.net"). 

-export([get_db_type/0, update_t/4, sql_transaction/2,
	 get_last/2, set_last_t/4, del_last/2, get_password/2,get_password_by_account/2,
	 set_password_t/3, add_user/3, del_user/2,
	 del_user_return_password/3, list_users/1, list_users/2,
	 users_number/1, users_number/2, add_spool_sql/2,
	 add_spool/2, get_and_del_spool_msg_t/2, del_spool_msg/2,
	 get_roster/2, get_roster_jid_groups/2,
	 get_roster_groups/3, del_user_roster_t/2,
	 get_roster_by_jid/3, get_rostergroup_by_jid/3,
	 del_roster/3, del_roster_sql/2, update_roster/5,
	 update_roster_sql/4, roster_subscribe/4,
	 get_subscription/3, set_private_data/4,
	 set_private_data_sql/3, get_private_data/3, get_private_data/2,
	 del_user_private_storage/2, get_default_privacy_list/2,
	 get_default_privacy_list_t/1, get_privacy_list_names/2,
	 get_privacy_list_names_t/1, get_privacy_list_id/3,
	 get_privacy_list_id_t/2, get_privacy_list_data/3,
	 get_privacy_list_data_by_id/2, get_privacy_list_data_t/2,
	 get_privacy_list_data_by_id_t/1,
	 set_default_privacy_list/2,
	 unset_default_privacy_list/2,
	 remove_privacy_list/2,
	 add_privacy_list/2,
	 set_privacy_list/2,
	 del_privacy_lists/3,
	 set_vcard/26,
	 get_vcard/2,
	 escape/1,
	 count_records_where/3,
	 get_roster_version/2,
	 set_roster_version/2]).

%% We have only two compile time options for db queries:
%-define(generic, true).
%-define(mssql, true).
%-ifndef(mssql).

%-undef(generic).

-define(generic, true).

%-endif.

-include("ejabberd.hrl").
-include("logger.hrl").
-include("odbc_auth.hrl").

%% Almost a copy of string:join/2.
%% We use this version because string:join/2 is relatively
%% new function (introduced in R12B-0).
join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].

%% -----------------
%% Generic queries
-ifdef(generic).

get_db_type() -> generic.

%% Safe atomic update.
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query_t([<<"update ">>, Table,
				    <<" set ">>, join(UPairs, <<", ">>),
				    <<" where ">>, Where, <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
	  ejabberd_odbc:sql_query_t([<<"insert into ">>, Table,
				     <<"(">>, join(Fields, <<", ">>),
				     <<") values ('">>, join(Vals, <<"', '">>),
				     <<"');">>])
    end.

update(LServer, Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
				   <<A/binary, "='", B/binary, "'">>
			   end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query(LServer,
				 [<<"update ">>, Table, <<" set ">>,
				  join(UPairs, <<", ">>), <<" where ">>, Where,
				  <<";">>])
	of
      {updated, 1} -> ok;
      _ ->
	  ejabberd_odbc:sql_query(LServer,
				  [<<"insert into ">>, Table, <<"(">>,
				   join(Fields, <<", ">>), <<") values ('">>,
				   join(Vals, <<"', '">>), <<"');">>])
    end.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from im_the ejabberd_odbc module to this one (odbc_queries)
sql_transaction(LServer, F) ->
    ejabberd_odbc:sql_transaction(LServer, F).

get_last(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select seconds, state from im_last where user_id='">>,UserID, <<"'">>]).

set_last_t(LServer, UserID, Seconds, State) ->
    update(LServer, <<"im_last">>,
	   [<<"user_id">>, <<"seconds">>, <<"state">>],
	   [UserID, Seconds, State],
	   [?AUTH_USER_USER_ID, UserID, <<"'">>]).

del_last(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from im_last where user_id='">>, UserID, <<"'">>]).

%% @author dungang
%% @date 2014-8-23
%% @description 通过邮箱或者手机号码验证密码
get_password_by_account(LServer, UserAccount) ->
    SQL = case string:chr(binary_to_list(UserAccount),$#) of 
        0 ->
        		[<<"select ">>,?AUTH_USER_PASSWORD,<<",">>,?AUTH_USER_USER_ID,<<",">>,?AUTH_USER_SALT,
				 <<" from ">>,?AUTH_TABLE,<<" where ">>,?AUTH_USER_MOBILE,<<"='">>,
        			     UserAccount, <<"';">>];
        _ ->
            [<<"select ">>,?AUTH_USER_PASSWORD,<<",">>,?AUTH_USER_USER_ID,<<",">>,?AUTH_USER_SALT,
				 <<" from ">>,?AUTH_TABLE,<<" where ">>,?AUTH_USER_EMAIL,<<"='">>,
                         ejabberd_regexp:replace(UserAccount,<<"#">>,<<"@">>), <<"';">>]
        
    end,
    ejabberd_odbc:sql_query(LServer,SQL).

get_password(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			[<<"select ">>,?AUTH_USER_PASSWORD,<<",">>,?AUTH_USER_USER_ID,<<",">>,?AUTH_USER_SALT,
				 <<" from ">>,?AUTH_TABLE,<<" where ">>,?AUTH_USER_USER_ID,<<"='">>,
        			     UserID, <<"';">>]).

set_password_t(LServer, UserID, Pass) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  update_t(?AUTH_TABLE,
						   [?AUTH_USER_USER_ID,
						    ?AUTH_USER_PASSWORD],
						   [UserID, Pass],
						   [?AUTH_USER_USER_ID,<<"='">>, UserID,
						    <<"'">>])
				  end).

add_user(LServer, UserID, Pass) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"insert into ">>,?AUTH_TABLE,<<" (">>,?AUTH_USER_USER_ID,
						    <<", ">>,?AUTH_USER_PASSWORD,<<") "
			       "values ('">>,
			     UserID, <<"', '">>, Pass, <<"');">>]).

del_user(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from ">>,?AUTH_TABLE,<<" where ">>,?AUTH_USER_USER_ID,<<"='">>, UserID,
			     <<"';">>]).

del_user_return_password(_LServer, UserID, Pass) ->
    P =
	ejabberd_odbc:sql_query_t([<<"select ">>,?AUTH_USER_PASSWORD,<<", ">>,?AUTH_USER_SALT,<<" from ">>,?AUTH_TABLE,<<" where ">>,?AUTH_USER_USER_ID,<<"='">>,
				   UserID, <<"';">>]),
	Fields = [?AUTH_USER_PASSWORD,?AUTH_USER_SALT],
	case P of 
		{select ,Fields,[[_,Salt]]} ->
		    ejabberd_odbc:sql_query_t([<<"delete from ">>,?AUTH_TABLE,<<" where ">>,?AUTH_USER_USER_ID,<<"='">>,
					       UserID, <<"' and ">>,?AUTH_USER_PASSWORD,<<"='">>, 
							md5_string:md5_hex(string:concat(Salt, Pass)),
					       <<"';">>])
	end,
    P.

list_users(LServer) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select ">>,?AUTH_USER_USER_ID,<<" from ">>,?AUTH_TABLE]).

list_users(LServer, [{from, Start}, {to, End}])
    when is_integer(Start) and is_integer(End) ->
    list_users(LServer,
	       [{limit, End - Start + 1}, {offset, Start - 1}]);
list_users(LServer,
	   [{prefix, Prefix}, {from, Start}, {to, End}])
    when is_binary(Prefix) and is_integer(Start) and
	   is_integer(End) ->
    list_users(LServer,
	       [{prefix, Prefix}, {limit, End - Start + 1},
		{offset, Start - 1}]);
list_users(LServer, [{limit, Limit}, {offset, Offset}])
    when is_integer(Limit) and is_integer(Offset) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:format(
                                 "select ~s from ~s " ++
                                     "order by ~s " ++
                                     "limit ~w offset ~w",
                                 [?AUTH_USER_USER_ID,?AUTH_TABLE,?AUTH_USER_USER_ID,Limit, Offset]))]);
list_users(LServer,
	   [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:format(
                                 "select ~s from ~s " ++
                                     "where ~s like '~s%' " ++
                                     "order by ~s " ++
                                     "limit ~w offset ~w ",
                                 [?AUTH_USER_USER_ID,?AUTH_TABLE,?AUTH_USER_USER_ID,
								  Prefix, ?AUTH_USER_USER_ID, Limit, Offset]))]).

users_number(LServer) ->
    Type = ejabberd_config:get_option({odbc_type, LServer},
                                      fun(pgsql) -> pgsql;
                                         (mysql) -> mysql;
                                         (odbc) -> odbc
                                      end, odbc),
    case Type of
      pgsql ->
	  case
	    ejabberd_config:get_option(
              {pgsql_users_number_estimate, LServer},
              fun(V) when is_boolean(V) -> V end,
              false)
	      of
	    true ->
		ejabberd_odbc:sql_query(LServer,
					[<<"select reltuples from im_pg_class where "
                                           "oid = 'users'::regclass::oid">>]);
	    _ ->
		ejabberd_odbc:sql_query(LServer,
					[<<"select count(*) from ">>,?AUTH_TABLE])
	  end;
      _ ->
	  ejabberd_odbc:sql_query(LServer,
				  [<<"select count(*) from ">>,?AUTH_TABLE])
    end.

users_number(LServer, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    ejabberd_odbc:sql_query(LServer,
			    [list_to_binary(
                               io_lib:fwrite(
                                 "select count(*) from ~s " ++
                                     %% Warning: Escape prefix at higher level to prevent SQL
                                     %%          injection.
                                     "where ~s like '~s%'",
                                 [?AUTH_TABLE,?AUTH_USER_USER_ID,Prefix]))]);
users_number(LServer, []) ->
    users_number(LServer).


add_spool_sql(UserID, XML) ->
    [<<"insert into im_spool(user_id, xml) values ('">>,
     UserID, <<"', '">>, XML, <<"');">>].

add_spool(LServer, Queries) ->
    ejabberd_odbc:sql_transaction(LServer, Queries).

get_and_del_spool_msg_t(LServer, UserID) ->
    F = fun () ->
		Result =
		    ejabberd_odbc:sql_query_t([<<"select user_id, xml from im_spool where "
						 "user_id='">>,
					       UserID,
					       <<"'  order by seq;">>]),
		ejabberd_odbc:sql_query_t([<<"delete from im_spool where user_id='">>,
					   UserID, <<"';">>]),
		Result
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

del_spool_msg(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from im_spool where user_id='">>, UserID,
			     <<"';">>]).

get_roster(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select user_id, jid, nick, subscription, "
			       "ask, askmessage, server, subscribe, "
			       "type from im_rosterusers where user_id='">>,
			     UserID, <<"'">>]).

get_roster_jid_groups(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select jid, grp from im_rostergroups where "
			       "user_id='">>,
			     UserID, <<"'">>]).

get_roster_groups(_LServer, UserID, SJID) ->
    ejabberd_odbc:sql_query_t([<<"select grp from im_rostergroups where user_id='">>,
			       UserID, <<"' and jid='">>, SJID, <<"';">>]).

del_user_roster_t(LServer, UserID) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  ejabberd_odbc:sql_query_t([<<"delete from im_rosterusers       where "
								       "user_id='">>,
								     UserID,
								     <<"';">>]),
					  ejabberd_odbc:sql_query_t([<<"delete from im_rostergroups       where "
								       "user_id='">>,
								     UserID,
								     <<"';">>])
				  end).

get_roster_by_jid(_LServer, UserID, SJID) ->
    ejabberd_odbc:sql_query_t([<<"select user_id, jid, nick, subscription, "
				 "ask, askmessage, server, subscribe, "
				 "type from im_rosterusers where user_id='">>,
			       UserID, <<"' and jid='">>, SJID, <<"';">>]).

get_rostergroup_by_jid(LServer, UserID, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select grp from im_rostergroups where user_id='">>,
			     UserID, <<"' and jid='">>, SJID, <<"'">>]).

del_roster(_LServer, UserID, SJID) ->
    ejabberd_odbc:sql_query_t([<<"delete from im_rosterusers       where "
				 "user_id='">>,
			       UserID, <<"'         and jid='">>, SJID,
			       <<"';">>]),
    ejabberd_odbc:sql_query_t([<<"delete from im_rostergroups       where "
				 "user_id='">>,
			       UserID, <<"'         and jid='">>, SJID,
			       <<"';">>]).

del_roster_sql(UserID, SJID) ->
    [[<<"delete from im_rosterusers       where "
	"user_id='">>,
      UserID, <<"'         and jid='">>, SJID, <<"';">>],
     [<<"delete from im_rostergroups       where "
	"user_id='">>,
      UserID, <<"'         and jid='">>, SJID, <<"';">>]].

update_roster(_LServer, UserID, SJID, ItemVals,
	      ItemGroups) ->
    update_t(<<"im_rosterusers">>,
	     [<<"user_id">>, <<"jid">>, <<"nick">>,
	      <<"subscription">>, <<"ask">>, <<"askmessage">>,
	      <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"user_id='">>, UserID, <<"' and jid='">>, SJID,
	      <<"'">>]),
    ejabberd_odbc:sql_query_t([<<"delete from im_rostergroups       where "
				 "user_id='">>,
			       UserID, <<"'         and jid='">>, SJID,
			       <<"';">>]),
    lists:foreach(fun (ItemGroup) ->
			  ejabberd_odbc:sql_query_t([<<"insert into im_rostergroups(           "
						       "   user_id, jid, grp)  values ('">>,
						     join(ItemGroup,
							  <<"', '">>),
						     <<"');">>])
		  end,
		  ItemGroups).

update_roster_sql(UserID, SJID, ItemVals,
		  ItemGroups) ->
    [[<<"delete from im_rosterusers       where "
	"user_id='">>,
      UserID, <<"'         and jid='">>, SJID, <<"';">>],
     [<<"insert into im_rosterusers(            "
	"  user_id, jid, nick,              "
	" subscription, ask, askmessage,     "
	"          server, subscribe, type)  "
	"values ('">>,
      join(ItemVals, <<"', '">>), <<"');">>],
     [<<"delete from im_rostergroups       where "
	"user_id='">>,
      UserID, <<"'         and jid='">>, SJID, <<"';">>]]
      ++
      [[<<"insert into im_rostergroups(           "
	  "   user_id, jid, grp)  values ('">>,
	join(ItemGroup, <<"', '">>), <<"');">>]
       || ItemGroup <- ItemGroups].

roster_subscribe(_LServer, UserID, SJID, ItemVals) ->
    update_t(<<"im_rosterusers">>,
	     [<<"user_id">>, <<"jid">>, <<"nick">>,
	      <<"subscription">>, <<"ask">>, <<"askmessage">>,
	      <<"server">>, <<"subscribe">>, <<"type">>],
	     ItemVals,
	     [<<"user_id='">>, UserID, <<"' and jid='">>, SJID,
	      <<"'">>]).

get_subscription(LServer, UserID, SJID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select subscription from im_rosterusers "
			       "where user_id='">>,
			     UserID, <<"' and jid='">>, SJID, <<"'">>]).

set_private_data(_LServer, UserID, LXMLNS, SData) ->
    update_t(<<"im_private_storage">>,
	     [<<"user_id">>, <<"namespace">>, <<"data">>],
	     [UserID, LXMLNS, SData],
	     [<<"user_id='">>, UserID, <<"' and namespace='">>,
	      LXMLNS, <<"'">>]).

set_private_data_sql(UserID, LXMLNS, SData) ->
    [[<<"delete from im_private_storage where user_id='">>,
      UserID, <<"' and namespace='">>, LXMLNS, <<"';">>],
     [<<"insert into im_private_storage(user_id, "
	"namespace, data) values ('">>,
      UserID, <<"', '">>, LXMLNS, <<"', '">>, SData,
      <<"');">>]].

get_private_data(LServer, UserID, LXMLNS) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select data from im_private_storage where "
			       "user_id='">>,
			     UserID, <<"' and namespace='">>, LXMLNS,
			     <<"';">>]).

get_private_data(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
                            [<<"select namespace, data from im_private_storage "
                               "where user_id='">>, UserID, <<"';">>]).

del_user_private_storage(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from im_private_storage where user_id='">>,
			     UserID, <<"';">>]).

set_vcard(LServer, LUserIDID, SBDay, SCTRY, SEMail, SFN,
	  SFamily, SGiven, SLBDay, SLCTRY, SLEMail, SLFN,
	  SLFamily, SLGiven, SLLocality, SLMiddle, SLNickname,
	  SLOrgName, SLOrgUnit, SLocality, SMiddle, SNickname,
	  SOrgName, SOrgUnit, SVCARD, UserID) ->
    ejabberd_odbc:sql_transaction(LServer,
				  fun () ->
					  update_t(<<"im_vcard">>,
						   [<<"user_id">>,
						    <<"vcard">>],
						   [LUserIDID, SVCARD],
						   [<<"user_id='">>, LUserIDID,
						    <<"'">>]),
					  update_t(<<"im_vcard_search">>,
						   [<<"user_id">>,
						    <<"lusername">>, <<"fn">>,
						    <<"lfn">>, <<"family">>,
						    <<"lfamily">>, <<"given">>,
						    <<"lgiven">>, <<"middle">>,
						    <<"lmiddle">>,
						    <<"nickname">>,
						    <<"lnickname">>, <<"bday">>,
						    <<"lbday">>, <<"ctry">>,
						    <<"lctry">>, <<"locality">>,
						    <<"llocality">>,
						    <<"email">>, <<"lemail">>,
						    <<"orgname">>,
						    <<"lorgname">>,
						    <<"orgunit">>,
						    <<"lorgunit">>],
						   [UserID, LUserIDID, SFN,
						    SLFN, SFamily, SLFamily,
						    SGiven, SLGiven, SMiddle,
						    SLMiddle, SNickname,
						    SLNickname, SBDay, SLBDay,
						    SCTRY, SLCTRY, SLocality,
						    SLLocality, SEMail, SLEMail,
						    SOrgName, SLOrgName,
						    SOrgUnit, SLOrgUnit],
						   [<<"luser_id='">>,
						    LUserIDID, <<"'">>])
				  end).

get_vcard(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select im_vcard from im_vcard where user_id='">>,
			     UserID, <<"';">>]).

get_default_privacy_list(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select name from im_privacy_default_list "
			       "where user_id='">>,
			     UserID, <<"';">>]).

get_default_privacy_list_t(UserID) ->
    ejabberd_odbc:sql_query_t([<<"select name from im_privacy_default_list "
				 "where user_id='">>,
			       UserID, <<"';">>]).

get_privacy_list_names(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select name from im_privacy_list where "
			       "user_id='">>,
			     UserID, <<"';">>]).

get_privacy_list_names_t(UserID) ->
    ejabberd_odbc:sql_query_t([<<"select name from im_privacy_list where "
				 "user_id='">>,
			       UserID, <<"';">>]).

get_privacy_list_id(LServer, UserID, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select id from im_privacy_list where user_id='">>,
			     UserID, <<"' and name='">>, SName, <<"';">>]).

get_privacy_list_id_t(UserID, SName) ->
    ejabberd_odbc:sql_query_t([<<"select id from im_privacy_list where user_id='">>,
			       UserID, <<"' and name='">>, SName, <<"';">>]).

get_privacy_list_data(LServer, UserID, SName) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select t, value, action, ord, match_all, "
			       "match_iq, match_message, match_presence_in, "
			       "match_presence_out from im_privacy_list_data "
			       "where id = (select id from im_privacy_list "
			       "where             user_id='">>,
			     UserID, <<"' and name='">>, SName,
			     <<"') order by ord;">>]).

get_privacy_list_data_t(UserID, SName) ->
    ejabberd_odbc:sql_query_t([<<"select t, value, action, ord, match_all, "
                                 "match_iq, match_message, match_presence_in, "
                                 "match_presence_out from im_privacy_list_data "
                                 "where id = (select id from im_privacy_list "
                                 "where             user_id='">>,
                               UserID, <<"' and name='">>, SName,
                               <<"') order by ord;">>]).

get_privacy_list_data_by_id(LServer, ID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select t, value, action, ord, match_all, "
			       "match_iq, match_message, match_presence_in, "
			       "match_presence_out from im_privacy_list_data "
			       "where id='">>,
			     ID, <<"' order by ord;">>]).

get_privacy_list_data_by_id_t(ID) ->
    ejabberd_odbc:sql_query_t([<<"select t, value, action, ord, match_all, "
				 "match_iq, match_message, match_presence_in, "
				 "match_presence_out from im_privacy_list_data "
				 "where id='">>,
			       ID, <<"' order by ord;">>]).

set_default_privacy_list(UserID, SName) ->
    update_t(<<"im_privacy_default_list">>,
	     [<<"user_id">>, <<"name">>], [UserID, SName],
	     [<<"user_id='">>, UserID, <<"'">>]).

unset_default_privacy_list(LServer, UserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from im_privacy_default_list    "
			       "   where user_id='">>,
			     UserID, <<"';">>]).

remove_privacy_list(UserID, SName) ->
    ejabberd_odbc:sql_query_t([<<"delete from im_privacy_list where user_id='">>,
			       UserID, <<"' and name='">>, SName, <<"';">>]).

add_privacy_list(UserID, SName) ->
    ejabberd_odbc:sql_query_t([<<"insert into im_privacy_list(user_id, name) "
				 "values ('">>,
			       UserID, <<"', '">>, SName, <<"');">>]).

set_privacy_list(ID, RItems) ->
    ejabberd_odbc:sql_query_t([<<"delete from im_privacy_list_data where "
				 "id='">>,
			       ID, <<"';">>]),
    lists:foreach(fun (Items) ->
			  ejabberd_odbc:sql_query_t([<<"insert into im_privacy_list_data(id, t, "
						       "value, action, ord, match_all, match_iq, "
						       "match_message, match_presence_in, match_prese"
						       "nce_out ) values ('">>,
						     ID, <<"', '">>,
						     join(Items, <<"', '">>),
						     <<"');">>])
		  end,
		  RItems).

del_privacy_lists(LServer, Server, UserID) ->
%% Characters to escape
%% Count number of records in a table given a where clause
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from im_privacy_list where user_id='">>,
			     UserID, <<"';">>]),
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from im_privacy_list_data where "
			       "value='">>,
			     <<UserID/binary, "@", Server/binary>>,
			     <<"';">>]),
    ejabberd_odbc:sql_query(LServer,
			    [<<"delete from im_privacy_default_list where "
			       "user_id='">>,
			     UserID, <<"';">>]).

escape($\000) -> <<"\\0">>;
escape($\n) -> <<"\\n">>;
escape($\t) -> <<"\\t">>;
escape($\b) -> <<"\\b">>;
escape($\r) -> <<"\\r">>;
escape($') -> <<"''">>;
escape($") -> <<"\\\"">>;
escape($\\) -> <<"\\\\">>;
escape(C) -> <<C>>.

count_records_where(LServer, Table, WhereClause) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select count(*) from ">>, Table, <<" ">>,
			     WhereClause, <<";">>]).

get_roster_version(LServer, LUserID) ->
    ejabberd_odbc:sql_query(LServer,
			    [<<"select version from im_roster_version where "
			       "user_id = '">>,
			     LUserID, <<"'">>]).

set_roster_version(LUserID, Version) ->
    update_t(<<"im_roster_version">>,
	     [<<"user_id">>, <<"version">>], [LUserID, Version],
	     [<<"user_id = '">>, LUserID, <<"'">>]).

-endif.