-module(mbox_box).
-behavior(gen_server).

-export([start_link/1, put_sync/2, put_async/2, set_sink/2,
         remove_sink/1, continue_delivery/1]).

-export([init/1, handle_call/3, handle_cast/2, code_change/3,
         terminate/2, handle_info/2]).

-record(state, {box = queue:new(),
                available,
                sink = not_set,
                sink_ref
               }).

-spec set_sink(Box :: pid(), Pid :: pid()) -> ok | {error, already_taken}.
set_sink(Box, Pid) ->
    gen_server:call(Box, {set_sink, Pid}).

-spec remove_sink(Box :: pid()) -> ok.
remove_sink(Box) ->
    gen_server:cast(Box, remove_sink).

continue_delivery(Box) ->
    gen_server:cast(Box, continue_delivery).

-spec put_sync(Box :: pid(), Message :: term()) -> ok | {error, full}.
put_sync(Box, Message) ->
    gen_server:call(Box, {put, Message}).

-spec put_async(Box :: pid, Message :: term()) -> ok.
put_async(Box, Message) ->
    gen_server:cast(Box, {put, Message}).

start_link(MaxDepth) ->
    gen_server:start_link(?MODULE, MaxDepth, []).

init(MaxDepth) ->
    {ok, #state{available = MaxDepth}}.

queue_message(Box, Message) ->
    case queue:is_empty(Box) of
        true ->
            self() ! deliver_message;
        false -> ok
    end,
    queue:in(Message, Box).

handle_call({put, _Message}, _From, S = #state{available = 0}) ->
    {reply, {error, full}, S};
handle_call({put, Message}, _From, S = #state{box = Box, available = N}) ->
    Q = queue_message(Box, Message),
    {reply, ok, S#state{box = Q, available = N-1}};
handle_call({set_sink, Pid}, _From, S) ->
    if S#state.sink =:= not_set ->
            Ref = erlang:monitor(process, Pid),
            self() ! deliver_message,
            {reply, ok, S#state{sink = Pid, sink_ref = Ref}};
       true ->
            {reply, {error, already_taken}, S}
    end.

handle_cast({put, Message}, S = #state{box = Box, available = 0}) ->
    {noreply, S#state{box = queue:in(Message, element(2, queue:out(Box)))}};
handle_cast({put, Message}, S = #state{box = Box, available = N}) ->
    Q = queue_message(Box, Message),
    {noreply, S#state{box = Q, available = N-1}};
handle_cast(remove_sink, S = #state{sink_ref=Ref}) ->
    erlang:demonitor(Ref),
    {noreply, S#state{sink = not_set, sink_ref = undefined}};
handle_cast(continue_delivery, State) ->
    self() ! deliver_message,
    {noreply, State}.

handle_info({'DOWN', Ref, _, _}, State = #state{sink_ref = Ref}) ->
    {noreply, State#state{sink = not_set, sink_ref = undefined}};
% it is possible that the process went down before we called
% demonitor. Just ignore 'DOWN' messages with the wron Ref.
handle_info({'DOWN', _, _, _}, State) ->
    {noreply, State};
handle_info(deliver_message, State=#state{sink = not_set}) ->
    {noreply, State};
handle_info(deliver_message, State=#state{box = Box, sink = Sink}) ->
    case queue:out(Box) of
        {{value, Message}, Q} ->
            Sink ! {box, self(), Message},
            {noreply, State#state{box = Q}};
        {empty, _Q} ->
            {noreply, State}
    end.

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
