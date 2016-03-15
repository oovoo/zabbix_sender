-module(zabbix_sender_srv).

-behaviour(gen_server).

-author("Valerii Vasilkov  <valerij.vasilkov@gmail.com>").

-include("zabbix_sender.hrl").

-export([
    start_link/0,
    start_link/1
]).

%% API
-export([
    start/3,
    start/4
]).

-export([
    send_packet/2,
    async_send_packet/2
]).

-export([
    send_msg/3,
    async_send_msg/3
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
    hostname :: binary() | string(),
    send_data :: packed_messages(),
    alias_name :: any()
}).


-ignore_xref([
  {start_link, 0},
  {start_link, 1},
  {handle_call, 3},
  {handle_cast, 2},
  {handle_info, 2},
  {terminate,2},
  {code_change,3}
]).


-spec send_msg(Sender, K, V) -> ok when
    Sender :: zabbix_sender(),
    K :: binary() | string(),
    V :: integer() | binary() | float() | string().
send_msg(Sender, Key, Val) ->
    send_packet(Sender, [{Key, Val}]).

-spec async_send_msg(Sender, K, V) -> ok when
    Sender :: zabbix_sender(),
    K :: binary() | string(),
    V :: integer() | binary() | float() | string().
async_send_msg(Sender, Key, Val) ->
    async_send_packet(Sender, [{Key, Val}]).

-spec async_send_packet(Sender, Packet) -> ok when
    Sender :: zabbix_sender(),
    Packet :: [Item],
    Item :: {K, V},
    K :: binary() | string(),
    V :: integer() | binary() | float() | string().
async_send_packet(Sender, [{_, _} | _] = Packet) ->
    case which_sender(Sender) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {send_packet, Packet});
        Reason ->
            {error, Reason}
    end.

-spec send_packet(Sender, Packet) -> ok when
    Sender :: zabbix_sender(),
    Packet :: [Item],
    Item :: {K, V},
    K :: binary() | string(),
    V :: integer() | binary() | float() | string().
send_packet(Sender, [{_, _} | _] = Packet) ->
    case which_sender(Sender) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {send_packet, Packet});
        Reason ->
            {error, Reason}
    end.


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_link(term()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


-spec start(tcp_host(), tcp_port(), string()) -> {ok, pid()}.
start(HostName, ZabbixHost, ZabbixPort) ->
    zabbix_sender_sup:start_sender([?MODULE, HostName, ZabbixHost, ZabbixPort]).

-spec start(any(),  string(), tcp_host(), tcp_port()) -> {ok, pid()}.
start(AliasName, HostName, ZabbixHost, ZabbixPort) ->
    zabbix_sender_sup:start_sender([AliasName, HostName, ZabbixHost, ZabbixPort]).

-spec init(list())-> {ok, #st{}}.
init([]) ->
    {ok, ZabbixHost} = application:get_env(zabbix_sender, zabbix_host),
    {ok, ZabbixPort} = application:get_env(zabbix_sender, zabbix_port),
    {ok, HostName} = application:get_env(zabbix_sender, hostname),
    init([?MODULE, HostName, ZabbixPort, ZabbixHost]);


init([AliasName, HostName, ZabbixHost, ZabbixPort]) ->
    zabbix_sender_register:add(AliasName),
    St = #st{host = ZabbixHost, port = ZabbixPort, hostname = HostName, alias_name = AliasName},
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
terminate(_Reason, _St = #st{alias_name = Name}) ->
    zabbix_sender_register:del(Name),
    ok.

-spec code_change(term(), #st{}, term()) -> {ok, #st{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internals
%% ===================================================================
process_packet(#st{port = ZabbixPort, host = ZabbixHost, hostname = NodeName}, Packet)->
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
  [{<<"key">>, cast_type(Key)}, {<<"value">>, value_fixtype(Value)}, {<<"host">>, cast_type(HostName)}].


value_fixtype(V) when is_list(V) -> list_to_binary(V);
value_fixtype(V) -> V.

cast_type(V) when is_atom(V)-> V;
cast_type(V) when is_binary(V)->V;
cast_type(V) when is_integer(V)-> integer_to_binary(V);
cast_type(V) when is_list(V)-> list_to_binary(V);
cast_type(V) when is_float(V)-> float_to_binary(V).

which_sender(Sender) when is_pid(Sender) ->
  Sender;
which_sender(Sender)->
  zabbix_sender_register:get(Sender).