-module(mbox_box_sup).
-behavior(supervisor).

-export([init/1, start_link/0]).
-export([create_box/0]).

-define(SUP_NAME, ?MODULE).

create_box() ->
    supervisor:start_child(?SUP_NAME, []).

start_link() ->
    supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 100},

    BoxSpec = #{id => box,
                restart => temporary,
                shutdown => 10000,
                type => worker,
                modules => [mbox_box],
                start => {mbox_box, start_link, [application:get_env(max_size, 100)]}},

    {ok, {SupFlags, [BoxSpec]}}.
