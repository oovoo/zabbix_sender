-module(zabbix_sender_sup).
-author("Igor Prots <prots.igor@gmail.com>").
-behaviour(supervisor).

-include("zabbix_sender.hrl").
%% API
-export([start_link/0]).
-export([start_sender/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {zabbix_sender_srv, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {zabbix_sender_srv, start_link, Args}, permanent, 5000, Type, [I]}).

-ignore_xref([
    {start_link, 0},
    {init, 1}
]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_sender(list())->
    {ok, pid()} | {error, term()}.
start_sender(Args)->
    AliasName = hd(Args),
    case zabbix_sender_register:get(AliasName) of
        StartedPid when is_pid(StartedPid)->
            supervisor:terminate_child(?MODULE, StartedPid);
        _ ->
            ok
    end,
    supervisor:start_child(?MODULE, [Args]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(list())->
    {ok, {{RestartStrategy :: supervisor:strategy(),
        MaxR            :: non_neg_integer(),
        MaxT            :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {sender, {zabbix_sender_srv, start_link, []},
        Restart, Shutdown, Type, [zabbix_sender_srv]},

    {ok, { SupFlags, [AChild]} }.

