%%%-------------------------------------------------------------------
%%% @author xjkjmac03 dungang 
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 八月 2014 下午1:17
%%%-------------------------------------------------------------------
-module(xep_mod_bots).
-author("xjkjmac03 dungang").

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([start/2,
  stop/1,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([route/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-define(SERVER, ?MODULE).
-define(PROCNAME, ejabberd_mod_botx).
-define(BOTNAME, xep_mod_bots).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  gen_server:start_link({local, Proc},?MODULE, [Host,Opts],[]).

start(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  ChildSpec = {Proc,
    {?MODULE, start_link,[Host,Opts]},
    temporary,
    1000,
    worker,
    [?MODULE]
  },
  supervisor:start_child(ejabberd_sup,ChildSpec).

stop(Host) ->
  Proc = gen_mod:get_module_proc(Host,?PROCNAME),
  gen_server:call(Proc,stop),
  supervisor:terminate_child(ejabberd_sup,Proc),
  supervisor:delete_child(ejabberd_sup,Proc).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Opts]) ->
  ?INFO_MSG("XEP_MOD_BOTS: starting bots",[]),
  MyHost = gen_mod:get_opt_host(Host, Opts,<<"bots.@HOST@">>),
  ejabberd_router:register_route(MyHost,{apply,?MODULE,route}),
  {ok,Host}.

handle_call(_Request, _From, Host) ->
  {reply, ok, Host}.

handle_cast(_Request, Host) ->
  {noreply, Host}.

handle_info(_Info, Host) ->
  {noreply, Host}.


terminate(_Reason, _Host) ->
  ok.

code_change(_OldVsn, Host, _Extra) ->
  {ok, Host}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

route(From, To, {xmlel, <<"presence">>,_,_} = Packet) ->
  case xml:get_tag_attr_s(<<"type">>,Packet) of
    <<"subscribe">> ->
      send_presence(To,From,"subscribe");
    <<"subscribed">> ->
      send_presence(To,From, "subscribed"),
      send_presence(To,From,"");
    <<"unsubscribe">> ->
      send_presence(To,From,"unsubscribed"),
      send_presence(To,From,"unsubscribe");
    <<"unsubscribed">> ->
      send_presence(To,From,"unsubscribed");
    <<"">> ->
      send_presence(To,From,"");
    <<"unavailable">> ->
      ok;
    <<"probe">> ->
      send_presence(To,From,"");
    _Other ->
      ?INFO_MSG("Other kind of presence ~n ~p",[Packet])
  end,
  ok;

route(From, To, {xmlel,<<"message">>,_,_} = Packet) ->
  case xml:get_subtag_cdata(Packet,<<"body">>) of
    <<"">> ->
      ok;
    Body ->
      case xml:get_tag_attr_s(<<"type">>,Packet) of
        <<"error">> ->
          ?ERROR_MSG("received error message ~n ~p ->; ~p~n~p",[From,To,Packet]);
        _ ->
          auto_relay(To,From,strip_bom(Body))
      end
  end,
  ok.

send_presence(From, To, "") ->
  ejabberd_router:route(From,To,{xmlet,<<"presence">>,[],[]});
send_presence(From, To, TypeStr) ->
  ejabberd_router:route(From,To, {xmlel, <<"presence">>,[{<<"type">>,TypeStr}],[]}).

auto_relay(From,To,Body) ->
  send_message(From,To,<<"chat">>,Body).

strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.

send_message(From, To, TypeStr, BodyStr) ->
  XmlBody = {xmlel, <<"message">>,
    [{<<"type">>, TypeStr},
      {<<"from">>, jlib:jid_to_string(From)},
      {<<"to">>, jlib:jid_to_string(To)}],
    [{xmlel, <<"body">>, [],
      [{xmlcdata, BodyStr}]}]},
  ejabberd_router:route(From, To, XmlBody).