-module(erlcrm114_test).

-include_lib("eunit/include/eunit.hrl").
-include("erlcrm114.hrl").

-define(assertBadArg(V), ?assertEqual({error, badarg}, V)).

option_test() ->
    ?assertBadArg(erlcrm114:new(badarg)),
    ?assertBadArg(erlcrm114:new([nonopt])),
    ?assertBadArg(erlcrm114:new([hyperspace, svm])),
    ?assertMatch({ok, _}, erlcrm114:new([hyperspace,
                                         string,
                                         unigram,
                                         unique,
                                         {regex, <<"[:graph:]+">>},
                                         {classes, [{"class", success}, "not-class"]}])).

sanity_test() ->
    sanity_test([osb]),
    sanity_test([osb, unigram]),
    sanity_test([osb, microgroom]),
    sanity_test([svm]),
    sanity_test([svm, string]),
    sanity_test([pca]),
    sanity_test([hyperspace]),
    sanity_test([entropy]),
    sanity_test([entropy, unique]),
    sanity_test([entropy, unique, crosslink]).

sanity_test(Opts) ->
    {ok, Ref} = erlcrm114:new(Opts),
    ?assertBadArg(erlcrm114:learn(Ref, <<"text">>, 2)),
    ?assertEqual(ok, erlcrm114:learn(Ref, <<"not-text">>, 1)),
    ?assertEqual(ok, erlcrm114:learn(Ref, <<"text">>)),
    ?assertMatch(#result{bestmatch=0}, erlcrm114:classify(Ref, <<"text">>, [detail])),
    ?assertMatch(#result{bestmatch=1}, erlcrm114:classify(Ref, <<"not-text">>)),
    ?assertBadArg(erlcrm114:classify(Ref, <<"la la la">>, [flag])),
    ?assertBadArg(erlcrm114:classify(Ref, "not la la la")).
