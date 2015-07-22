-module(zabbix_sender).
-author("Valerii Vasilkov  <valerij.vasilkov@gmail.com>").

-export([start/0, stop/0, send_message/1, send_message/2, async_send_message/1, async_send_message/2]).

-spec start() -> ok.
start() ->
    application:start(inets),
    application:start(?MODULE),
    ok.

-spec stop() -> ok.
stop() ->
    application:stop(zabbix_sender).

-spec send_message(Packet) -> ok when
  Packet :: [Item],
  Item :: {K, V},
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
send_message(Packet)->
  zabbix_sender_srv:send_msg(Packet).

-spec send_message(K, V) -> ok when
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
send_message(K, V)->
  zabbix_sender_srv:async_send_msg(K, V).

-spec async_send_message(K, V) -> ok when
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
async_send_message(K, V)->
  zabbix_sender_srv:async_send_msg(K, V).

-spec async_send_message(Packet) -> ok when
  Packet :: [Item],
  Item :: {K, V},
  K :: binary() | string(),
  V :: integer() | binary() | float() | string().
async_send_message(Packet)->
  zabbix_sender_srv:send_msg(Packet).