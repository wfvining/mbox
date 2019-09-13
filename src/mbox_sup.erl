-module(mbox_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 100},

    ServerSpec = #{id => mbox_serv,
                   start => {mbox_serv, start_link, []},
                   restart => permanent,
                   shutdown => 50000,
                   type => worker,
                   modules => [mbox_serv]},

    BoxSupervisor = #{id => mbox_box_sup,
                      start => {mbox_box_sup, start_link, []},
                      restart => permanent,
                      shutdown => 50000,
                      type => supervisor,
                      modules => [mbox_box_sup]},

    {ok, {SupFlags, [ServerSpec, BoxSupervisor]}}.
