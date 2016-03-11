-module(zabbix_sender_register).
-author("valerii.vasylkov").

-define(Zabbix_Sender_Register, zabbix_senders_register).

%% API
-export([init/0]).

-export([
    add/1,
    del/1,
    get/1,
    get_name/0,
    get_name/1
]).



-spec init()-> atom().
init() ->
    ets:new(?Zabbix_Sender_Register, [named_table, public, {read_concurrency, true}]).

-spec add(any())-> true.
add(Name)->
    ets:insert(?Zabbix_Sender_Register, {Name, self()}).

-spec del(any())-> true.
del(Name)->
    ets:delete(?Zabbix_Sender_Register, Name).

-spec get(any())-> pid() | sender_not_found.
get(Name)->
    case ets:lookup(?Zabbix_Sender_Register, Name) of
        [{_, Pid}] when is_pid(Pid)->
            Pid;
        _ -> sender_not_found
    end.

-spec get_name()-> any().
get_name()->
    get_name(self()).

-spec get_name(pid())-> any().
get_name(Pid)->
    Pattern = [{{'$1', '$2'}, [{'=:=', '$2', Pid}],['$1']}],
    [Name] = ets:select(?Zabbix_Sender_Register, Pattern),
    Name.