-module(zabbix_sender).
-author("Valerii Vasilkov  <valerij.vasilkov@gmail.com>").

-export([start/0, stop/0]).

-spec start() -> ok.
start() ->
    application:start(inets),
    application:start(?MODULE),
    ok.

-spec stop() -> ok.
stop() ->
    application:stop(zabbix_sender).