-module(zabbix_sender_srv).

-behaviour(gen_server).

-author("Valerii Vasilkov  <valerij.vasilkov@gmail.com>").

-include("zabbix_sender.hrl").


%% API
-export([
  start_link/0,
  send_msg/1,
  send_msg/2,
  async_send_msg/1,
  async_send_msg/2
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(st, {
  host :: tcp_host(),
  port :: tcp_port(),
  socket :: socket_conn(),
  nodename :: binary() | string(),
  send_data :: packed_messages()
}).


-ignore_xref([
  {start_link, 0},
  {handle_call, 3},
  {handle_cast, 2},
  {handle_info, 2},
  {terminate,2},
  {code_change,3}
]).

-spec send_msg(Packet) -> ok when
  Packet :: [Item],
  Item :: {K, V},
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
send_msg([{_, _} | _] = Packet) ->
  gen_server:call(?MODULE, {send_packet, Packet}).

-spec send_msg(K, V) -> ok when
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
send_msg(Key, Val) ->
  gen_server:call(?MODULE, {send_packet, [{Key, Val}]}).

-spec async_send_msg(K, V) -> ok when
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
async_send_msg(Key, Val) ->
  gen_server:cast(?MODULE, {send_packet, [{Key, Val}]}).

-spec async_send_msg(Packet) -> ok when
  Packet :: [Item],
  Item :: {K, V},
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
async_send_msg([{_, _} | _] = Packet) ->
  gen_server:cast(?MODULE, {send_packet, Packet}).



-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) ->
  {ok, #st{}}.
init([]) ->
  {ok, ZabbixHost} = application:get_env(zabbix_sender, zabbix_host),
  {ok, ZabbixPort} = application:get_env(zabbix_sender, zabbix_port),
  {ok, ServerName} = application:get_env(zabbix_sender, nodename),
  St = #st{host = ZabbixHost, port = ZabbixPort, nodename = ServerName},
  lager:debug("zabbix_sender_srv inited with st: ~p~n", [St]),
  {ok, St}.


-spec handle_call(Req, From, St) -> Resp when
  Req :: {send_packet, Packet :: [{binary(), binary()}]},
  From :: reference(),
  St :: #st{},
  Resp :: {reply, Resp :: binary(), #st{}} | {stop, {unexpected_call, any()}, #st{}}.

handle_call({send_packet, Packet},_From, St ) ->
  case process_packet(St , Packet) of
    {ok, Res} ->
      {reply, Res, St};
    {error, Reason} ->
      {stop, normal, {error, Reason}, St}
  end;
handle_call(Msg, _From, State) ->
  {stop, {unexpected_call, Msg}, State}.

-spec handle_cast(Req, St) -> Resp when
  Req :: {send_packet, Packet :: [{binary(), binary()}]},
  St :: #st{},
  Resp :: {noreply, Resp :: binary(), #st{}} | {stop, {unexpected_cast, any()}, #st{}}.
handle_cast({send_packet, Packet}, St) ->
  case process_packet(St, Packet) of
    {ok, _} ->
      {noreply, St};
    {error, Reason} ->
      {stop, {shutdown, {zabbix_sender_error, Reason}}, St}
  end;
handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

-spec handle_info(Req, St) -> Resp when
  Req :: 'NOT FOR USE',
  St :: #st{},
  Resp :: {noreply, Resp :: binary(), #st{}} | {stop, {unexpected_info, any()}, #st{}}.
handle_info(Msg, St) ->
  {stop, {unexpected_info, Msg}, St}.

-spec terminate(term(), #st{}) -> ok.
terminate(_Reason, _St = #st{}) ->
  ok.

-spec code_change(term(), #st{}, term()) -> {ok, #st{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ===================================================================
%% Internals
%% ===================================================================
process_packet(#st{port = ZabbixPort, host = ZabbixHost, nodename = NodeName}, Packet)->
  case gen_tcp:connect(ZabbixHost, ZabbixPort,
    [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
    {ok, Socket} ->
      Resp = send_zabbix_packet(Packet, Socket, NodeName),
      ok = gen_tcp:close(Socket),
      Resp;
    {error, Reason} ->
      lager:error("can't connect to zabbix with reason ~p~n", [Reason]),
      {error, Reason}
  end.

send_zabbix_packet([{_, _} | _] = Packet, Socket, NodeName)  ->
  Request = prepare_packet(Packet, NodeName),
  gen_tcp:send(Socket, Request),
  case gen_tcp:recv(Socket, 0, 5000) of
    {ok, Resp} ->
      parse_resp(Resp),
      {ok, Resp};
    {error, Reason} ->
      lager:error("zabbix_sender error: ~p~n", [Reason]),
      {error, Reason}
  end.

parse_resp(<<_Header:5/binary, _Length:8/binary, Body/binary>>) ->
  lager:debug("zabbix_sender resp: ~ts~n", [Body]),
  Body;
parse_resp(<<"ZBXD", 1:8/integer>>)->
  <<"ACCEPTED!">>.


prepare_packet(Packet, NodeName) ->
  Items = [ prepare_item(PacketItem, NodeName) || PacketItem <- Packet],
  JsonMessage = to_json(Items),
  lager:debug("req json message ~ts~n", [JsonMessage]),
  Length = byte_size(JsonMessage),
  SizeHeader = <<Length:64/little-integer>>,
  <<?Zabbix_Header/binary, SizeHeader/binary, JsonMessage/binary>>.

to_json(Items) ->
  to_server_json(which_parser(), Items).

to_server_json(jsx, Items) ->
  jsx:encode(
    [{<<"request">>, <<"sender data">>},
      {<<"data">>, Items}]
  );
to_server_json(jiffy, Items) ->
  jiffy:encode(
    {[{<<"request">>, <<"sender data">>},
      {<<"data">>, wrap_eep18(Items)}]}
  );
to_server_json(jsone, Items) ->
  jsone:encode(
    {[{<<"request">>, <<"sender data">>},
      {<<"data">>, wrap_eep18(Items)}]}
  ).

which_parser() ->
  case is_module_exists(jiffy) of
    true -> jiffy;
    _ ->
      case is_module_exists(jsx) of
        true -> jsx;
        _ ->
          case is_module_exists(jsone) of
            true ->
              jsone;
            _ -> throw({error, no_parsers_available})
          end
      end
  end.

wrap_eep18(Items) ->
  lists:map(fun
              ([_Item | _] = Obj) when is_tuple(_Item) -> {Obj};
              (Obj) -> Obj
            end, Items).

is_module_exists(Module) ->
  case is_atom(Module) of
    true ->
      try Module:module_info() of
        _InfoList ->
          true
      catch
        _:_ ->
          false
      end;
    false ->
      false
  end.

prepare_item({Key, Value}, HostName) ->
  [{<<"key">>, to_binary(Key)}, {<<"value">>, value_fixtype(Value)}, {<<"host">>, to_binary(HostName)}].


value_fixtype(V) when is_list(V) -> list_to_binary(V);
value_fixtype(V) -> V.

to_binary(V) when is_binary(V)->V;
to_binary(V) when is_integer(V)-> integer_to_binary(V);
to_binary(V) when is_list(V)-> list_to_binary(V);
to_binary(V) when is_float(V)-> float_to_binary(V).