-module(zabbix_sender_app).
-author("Valerii Vasilkov  <valerij.vasilkov@gmail.com>").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
    {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.
start(_StartType, _StartArgs) ->
    zabbix_sender_register:init(),
    zabbix_sender_sup:start_link().

-spec stop(State :: term()) ->
    term().
stop(_State) ->
    ok.
