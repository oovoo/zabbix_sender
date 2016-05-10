zabbix_sender
=============

Simple native Zabbix_sender for Erlang applications    

[![Build Status](https://travis-ci.org/stalkermn/zabbix_sender.svg?branch=master)](https://travis-ci.org/stalkermn/zabbix_sender.svg?branch=master)

Getting Started
=======
### Building and compiling
#### rebar.config
```erl
{deps, [
    .....
    {zabbix_sender, ".*", {git, "https://github.com/stalkermn/zabbix_sender_.git", "master"}}
]}.
```
#### erlang.mk (Makefile)
This package is already in erlang.mk packages index. You can use it without specifying any references to original repositories
```sh
DEPS = zabbix_sender
```

##### Notice

Don't forget to include zabbix_sender name to your `*.app.src` OR `reltool.config` OR `relx.config` for properly including this lib to your release 


### Starting zabbix_sender
For starting using zabbix_sender      
```erl
application:start(lager).
application:start(jiffy). % (Optional) if you want use jiffy instead of jsx (by default)
application:start(inets).
application:start(zabbix_sender)
```

OR     

```erl
zabbix_sender:start()
```

OR if you have included properly to release, your zabbix_sender will be started automatically. 

### Usage    
Your main module for any manipulation is a `zabbix_sender_srv`

Before starting we should specify properly configuration of the zabbix_server:
```erl
zabbix_sender_srv:start(Hostname :: string(), tcp_host(), tcp_port()) -> {ok, pid()}
```
OR if you want to use multiply hosts, try to start your sender:    
```erl
zabbix_sender_srv:start(SenderName :: any(),  Hostname :: string(), tcp_host(), tcp_port()) -> {ok, pid()}.
```
Where `SenderName` is a name of this sender (will be used in future for sending data to zabbix)

Delivering metrics to zabbix:
For `zabbix_sender_srv:start/3`     
```erl
zabbix_sender_srv:send_packet(zabbix_sender_srv, [{key, value}]).
zabbix_sender_srv:async_send_packet(zabbix_sender_srv, [{key, value}]).
zabbix_sender_srv:send_msg(zabbix_sender_srv, key, value).
zabbix_sender_srv:async__send_msg(zabbix_sender_srv, key, value).
```

For `zabbix_sender_srv:start/4`     

```erl
zabbix_sender_srv:send_packet(SenderName :: any(), [{key, value}]).
zabbix_sender_srv:async_send_packet(SenderName :: any(), [{key, value}]).
zabbix_sender_srv:send_msg(SenderName :: any(), key, value).
zabbix_sender_srv:async__send_msg(SenderName :: any(), key, value).
```

## Contributing

All contributions are welcome: ideas, patches, documentation, bug reports, complaints, and even something you drew up on a napkin.

License
----
MIT
