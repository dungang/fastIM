%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_auth_odbc).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0,
	 plain_password_required/0]).

-include("ejabberd.hrl").

-include("logger.hrl").
-include("odbc_auth.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) -> ok.

plain_password_required() -> false.

store_type() -> plain.



%% @spec (User, Server, Password) -> {true,Newusername} | false | {error, Error}
check_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
      error -> false;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  Fields = [?AUTH_USER_PASSWORD,?AUTH_USER_USER_ID,?AUTH_USER_SALT],
	  try odbc_queries:get_password_by_account(LServer, Username) of
	    {selected, Fields, [[ServerPassword,UserID,Salt]]} ->
            InPassword = case is_bitstring(Password) of 
                true -> binary_to_list(Password);
                false -> Password
            end,
            UPassword = md5_string:md5_hex(string:concat(binary_to_list(Salt),InPassword)),
    		    case UPassword == binary_to_list(ServerPassword) of
                true  -> {true,UserID};
                false -> false
            end;
%% 		  false; %% Password is not correct
	    {selected, Fields, []} ->
		  false; %% Account does not exist
	    {error, _Error} ->
		  false %% Typical error is that table doesn't exist
	  catch
	    _:_ ->
		false %% Typical error is database not accessible
	  end
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> true | false | {error, Error}
check_password(User, Server, Password, _Digest,
	       _DigestGen) ->
        check_password(User, Server, Password).

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
      error -> {error, invalid_jid};
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  Pass = ejabberd_odbc:escape(Password),
	  LServer = jlib:nameprep(Server),
	  case catch odbc_queries:set_password_t(LServer,
						 Username, Pass)
	      of
	    {atomic, ok} -> ok;
	    Other -> {error, Other}
	  end
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid}
try_register(User, Server, Password) ->
    case jlib:nodeprep(User) of
      error -> {error, invalid_jid};
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  Pass = ejabberd_odbc:escape(Password),
	  LServer = jlib:nameprep(Server),
	  case catch odbc_queries:add_user(LServer, Username,
					   Pass)
	      of
	    {updated, 1} -> {atomic, ok};
	    _ -> {atomic, exists}
	  end
    end.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(fun (Server) ->
			  get_vh_registered_users(Server)
		  end,
		  Servers).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
	Fields = [?AUTH_USER_USER_ID],
    case catch odbc_queries:list_users(LServer) of
      {selected, Fields, Res} ->
	  [{U, LServer} || [U] <- Res];
      _ -> []
    end.

get_vh_registered_users(Server, Opts) ->
    LServer = jlib:nameprep(Server),
	Fields = [?AUTH_USER_USER_ID],
    case catch odbc_queries:list_users(LServer, Opts) of
      {selected, Fields, Res} ->
	  [{U, LServer} || [U] <- Res];
      _ -> []
    end.

get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer) of
      {selected, [_], [[Res]]} ->
	  jlib:binary_to_integer(Res);
      _ -> 0
    end.

get_vh_registered_users_number(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer, Opts) of
      {selected, [_], [[Res]]} ->
	  jlib:binary_to_integer(Res);
      _Other -> 0
    end.

get_password(User, Server) ->
    case jlib:nodeprep(User) of
      error -> false;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
      Fields = [?AUTH_USER_PASSWORD,?AUTH_USER_USER_ID,?AUTH_USER_SALT],
	  case catch odbc_queries:get_password(LServer, Username) of
		  {selected, Fields, [[ServerPassword,_UserID,_Salt]]} ->
			  ServerPassword;
	      _ -> false
	  end
    end.

get_password_s(User, Server) ->
    case jlib:nodeprep(User) of
      error -> <<"">>;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
      Fields = [?AUTH_USER_PASSWORD,?AUTH_USER_USER_ID,?AUTH_USER_SALT],
	  case catch odbc_queries:get_password(LServer, Username)
	      of
		  {selected, Fields, [[ServerPassword,_UserID,_Salt]]} ->
			  ServerPassword;
	      _ -> <<"">>
	  end
    end.


%% @spec (UserAccount, Server) -> true | false | {error, Error}
is_user_exists(UserAccount, Server) ->
    case jlib:nodeprep(UserAccount) of
      error -> false;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  Fields = [?AUTH_USER_PASSWORD,?AUTH_USER_USER_ID,?AUTH_USER_SALT],
	  try odbc_queries:get_password(LServer, Username) of
	    	{selected,  Fields, [[_,_,_]]} ->
				true; %% Account exists
	    	{selected, Fields, []} ->
				false; %% Account does not exist
	    	{error, Error} -> {error, Error}
	  catch
	    _:B -> {error, B}
	  end
    end.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(User, Server) ->
    case jlib:nodeprep(User) of
      error -> error;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  LServer = jlib:nameprep(Server),
	  catch odbc_queries:del_user(LServer, Username),
	  ok
    end.

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    case jlib:nodeprep(User) of
      error -> error;
      LUser ->
	  Username = ejabberd_odbc:escape(LUser),
	  Pass = ejabberd_odbc:escape(Password),
	  LServer = jlib:nameprep(Server),
	  Fields = [?AUTH_USER_PASSWORD],
	  F = fun () ->
		      Result = odbc_queries:del_user_return_password(LServer,
								     Username,
								     Pass),
		      case Result of
			{selected, Fields, [[Password]]} -> ok;
			{selected, Fields, []} -> not_exists;
			_ -> not_allowed
		      end
	      end,
	  {atomic, Result} = odbc_queries:sql_transaction(LServer,
							  F),
	  Result
    end.
