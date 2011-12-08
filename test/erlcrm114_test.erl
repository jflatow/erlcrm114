-module(erlcrm114_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, Ref} = erlcrm114:new(),
    ?assertEqual(ok, erlcrm114:fnc(Ref)).

