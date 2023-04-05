-module(exec).

-export([sync/2, sync/3, drop_port/1]).

-include("eknife.hrl").

-spec sync(list() | binary(), pid() | undefined) -> ok |
                                                    {error, integer()} |
                                                    {error, no_port}.

sync(Cmd, TopProc) -> sync(Cmd, TopProc, []).

-spec sync(list() | binary(), pid() | undefined,
           list()) -> ok | {error, integer()} | {error, no_port}.

sync(Cmd, TopProc, MoreOptions) ->
    case erlang:open_port({spawn, cast:to_list(Cmd)},
                          [exit_status] ++ MoreOptions)
        of
        Port when is_port(Port) -> sync_loop(Port, TopProc);
        Error ->
            ?LOG_ERROR("Can't start port for command ~p - ~p",
                       [Cmd, Error]),
            {error, no_port}
    end.

sync_loop(Port, TopProc) ->
    receive
        {Port, {exit_status, 0}} -> ok;
        {Port, {exit_status, Status}} -> {error, Status};
        Message when is_pid(TopProc) ->
            ?LOG_DEBUG("Forward ~p to ~p", [Message, TopProc]),
            TopProc ! Message,
            sync_loop(Port, TopProc);
        Message ->
            ?LOG_DEBUG("Ignore message ~p", [Message]),
            sync_loop(Port, TopProc)
    end.

-spec drop_port(undefined | port()) -> ok | undefined.

drop_port(undefined) -> undefined;
drop_port(Port) ->
    case erlang:port_info(Port) of
        undefined -> undefined;
        PList ->
            case proplists:get_value(os_pid, PList, undefined) of
                undefined -> erlang:port_close(Port);
                Pid ->
                    os:cmd(cast:to_list(io_libc:format("kill -9 %d",
                                                       [Pid])))
            end,
            ok
    end.
