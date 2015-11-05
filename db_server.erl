-module(db_server).
-compile(export_all).

-import(lists, [foreach/2]).

%% IMPORTANT: The next line must be included
%%            if we want to call qlc:q(...)

-include_lib("stdlib/include/qlc.hrl").

-record(eigen, {}).
-record(std, {name, price}).
-record(print, {id, plan}).


start() ->
    mnesia:start(),
    mnesia:create_table(eigen,   [{attributes, record_info(fields, eigen)}]),
    mnesia:create_table(std,   [{attributes, record_info(fields, std)}]),
    mnesia:create_table(print, [{attributes, record_info(fields, print)}]),
    mnesia:wait_for_tables([shop,cost,design], 20000).

stop() -> mnesia:stop().

example_tables() ->
    [%% The shop table
     {shop, apple,   20,   2.3},
     {shop, orange,  100,  3.8},
     {shop, pear,    200,  3.6},
     {shop, banana,  420,  4.5},
     {shop, potato,  2456, 1.2},
     %% The cost table
     {cost, apple,   1.5},
     {cost, orange,  2.4},
     {cost, pear,    2.2},
     {cost, banana,  1.5},
     {cost, potato,  0.6}
    ].

reset_tables() ->
    mnesia:clear_table(shop),
    mnesia:clear_table(cost),
    F = fun() ->
        foreach(fun mnesia:write/1, example_tables())
    end,
    mnesia:transaction(F).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

demo(select_shop) ->
    do(qlc:q([X || X <- mnesia:table(shop)])).