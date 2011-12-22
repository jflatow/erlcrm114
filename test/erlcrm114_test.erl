-module(erlcrm114_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, Ref} = erlcrm114:new(),
    ?assertEqual(ok, erlcrm114:learn(Ref, <<>>)).

option_test() ->
    {ok, Ref} = erlcrm114:new([hyperspace,
                               {regex, <<"[:graph:]+">>},
                               {classes, ["class", "not-class"]}]),
    ?assertEqual(ok, erlcrm114:learn(Ref, <<>>)).
