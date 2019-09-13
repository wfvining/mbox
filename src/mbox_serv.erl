-module(mbox_serv).

-behavior(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0, register_box/1, get_box/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {boxes=#{}}).

register_box(Box) ->
    gen_server:call(?SERVER, {register, Box}).

get_box(Box) ->
    gen_server:call(?SERVER, {get, Box}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

find_or_create(Box, Boxes) ->
    case maps:get(Box, Boxes, nokey) of
        nokey ->
            {ok, NewBox} = mbox_box_sup:create_box(),
            {NewBox, Boxes#{Box => {NewBox, erlang:monitor(process, NewBox)}}};
        {B, _} ->
            {B, Boxes}
    end.

handle_call({get, Box}, _From, State) ->
    {B, NewBoxes} = find_or_create(Box, State#state.boxes),
    {reply, B, State#state{boxes=NewBoxes}};
handle_call({register, BoxName}, _From, State) ->
    {Box, Boxes} = find_or_create(BoxName, State#state.boxes),
    {reply, Box, State#state{boxes = Boxes}}.

handle_cast(Request, State) ->
    io:format("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info(Request, State) ->
    io:format("Unexpected message: ~p~n", [Request]),
    {noreply, State}.

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
