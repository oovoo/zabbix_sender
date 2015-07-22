%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <Privatbakn>
%%% @doc
%%%
%%% @end
%%% Created : 06. Март 2015 12:00
%%%-------------------------------------------------------------------
-author("protoj").

-define(Zabbix_Header, <<"ZBXD", 1:8/integer>>).
-type tcp_host() :: inet:ip_address() | inet:hostname().
-type tcp_port() :: inet:port_number().
-type socket_conn() :: inet:socket().
-type miliseconds() :: non_neg_integer().
-type packed_message() :: iolist().
-type packed_messages() :: [packed_message()].
-type socket_monitor() :: reference().