-include_lib("kernel/include/logger.hrl").

% timer

-define(S2MS(S), S * 1000).
-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).

% flow

-define(ASYNC(F), proc_lib:spawn(fun () -> F end)).

% pub/sub (gproc)

-define(ME(Reg), pubsub_gproc:me(Reg)).
-define(LOOKUP(Reg), pubsub_gproc:lookup(Reg)).
-define(LOOKUPS(Reg), pubsub_gproc:lookups(Reg)).
-define(PUB(Event, Msg), pubsub_gproc:pub(Event, Msg)).
-define(SUB(Event), pubsub_gproc:sub(Event)).
-define(UNSUB(Event), pubsub_gproc:unsub(Event)).
-define(LOOKUP_SUB(Reg), pubsub_gproc:lookup_sub(Reg)).
-define(IS_SUB(Event), pubsub_gproc:is_sub(Reg)).
