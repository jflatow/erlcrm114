-module(erlcrm114_test).

-include_lib("eunit/include/eunit.hrl").
-include("erlcrm114.hrl").

-define(assertBadArg(V), ?assertEqual({error, badarg}, V)).

-define(test_dir(Path), filename:join(code:lib_dir(erlcrm114, test), Path)).
-define(zip_record(Type, Record), lists:zip([type|record_info(fields, Type)], tuple_to_list(Record))).

%% io:format(standard_error, "~p~n", [?zip_record(result, Result)]).

list_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    [filename:join(Dir, File) || File <- Files].

load_classifier(Name, Opts) ->
    {Classes, LearnSpecs} = read_classes(?test_dir(Name ++ ".cls")),
    lists:foldl(fun ({Text, Index}, {ok, Cls}) ->
                        erlcrm114:learn(Cls, Text, Index)
                end, erlcrm114:new([{classes, Classes}|Opts]), LearnSpecs).

read_classes(ClassifierDir) ->
    read_classes(list_dir(ClassifierDir), {array:new(), []}).

read_classes([ClassDir|ClassDirs], {Classes, LearnSpecs}) ->
    {Index, Class} =
        case string:tokens(filename:basename(ClassDir), "_") of
            [I] ->
                {list_to_integer(I), I};
            [I, Name] ->
                {list_to_integer(I), Name};
            [I, Name, Direction] ->
                {list_to_integer(I), {Name, list_to_atom(Direction)}}
        end,
    read_classes(ClassDirs, {array:set(Index, Class, Classes),
                             [case file:read_file(Doc) of
                                  {ok, Data} -> {Data, Index}
                              end || Doc <- list_dir(ClassDir)] ++ LearnSpecs});
read_classes([], {Classes, LearnSpecs}) ->
    {array:to_list(Classes), LearnSpecs}.

read_doc(Doc) ->
    {ok, Data} = file:read_file(filename:join(?test_dir("docs"), Doc)),
    Data.

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
    {ok, Cls} = erlcrm114:new(Opts),
    ?assertBadArg(erlcrm114:learn(Cls, <<"text">>, 2)),
    ?assertEqual({ok, Cls}, erlcrm114:learn(Cls, <<"not-text">>, 1)),
    ?assertEqual({ok, Cls}, erlcrm114:learn(Cls, ["text"])),
    ?assertMatch(#result{bestmatch=0}, erlcrm114:classify(Cls, ["t", <<"ext">>], [detail])),
    ?assertMatch(#result{bestmatch=1}, erlcrm114:classify(Cls, <<"not-text">>)),
    ?assertBadArg(erlcrm114:classify(Cls, <<"la la la">>, [flag])).

serdes_test() ->
    {ok, Orig} = erlcrm114:new(),
    ?assertBadArg(erlcrm114:learn(Orig, <<"text">>, 2)),
    ?assertEqual({ok, Orig}, erlcrm114:learn(Orig, <<"not-text">>, 1)),
    ?assertEqual({ok, Orig}, erlcrm114:learn(Orig, <<"text">>)),
    {ok, Copy} = erlcrm114:from_binary(erlcrm114:to_binary(Orig)),
    ?assertMatch(#result{bestmatch=0}, erlcrm114:classify(Copy, <<"text">>, [detail])),
    ?assertMatch(#result{bestmatch=1}, erlcrm114:classify(Copy, <<"not-text">>)).

classic_test(Opts) ->
    {ok, Cls} = load_classifier("classic", Opts),
    ?assertMatch(#result{bestmatch=0}, erlcrm114:classify(Cls, read_doc("macbeth_frag.txt"))),
    ?assertMatch(#result{bestmatch=1}, erlcrm114:classify(Cls, read_doc("alice_frag.txt"))).

four_class_test(Opts) ->
    {ok, Cls} = load_classifier("four-class", Opts),
    ?assertMatch(#result{bestmatch=0}, erlcrm114:classify(Cls, read_doc("alice_frag.txt"))),
    ?assertMatch(#result{bestmatch=1}, erlcrm114:classify(Cls, read_doc("hound_frag.txt"))),
    ?assertMatch(#result{bestmatch=2}, erlcrm114:classify(Cls, read_doc("macbeth_frag.txt"))),
    ?assertMatch(#result{bestmatch=3}, erlcrm114:classify(Cls, read_doc("willows_frag.txt"))).

maturity_test(Opts) ->
    {ok, Cls} = load_classifier("maturity", Opts),
    ?assertMatch(#result{bestmatch=0}, erlcrm114:classify(Cls, read_doc("alice_frag.txt"))),
    ?assertMatch(#result{bestmatch=1}, erlcrm114:classify(Cls, read_doc("hound_frag.txt"))),
    ?assertMatch(#result{bestmatch=1}, erlcrm114:classify(Cls, read_doc("macbeth_frag.txt"))),
    ?assertMatch(#result{bestmatch=0}, erlcrm114:classify(Cls, read_doc("willows_frag.txt"))).

two_two_test(Opts) ->
    two_two_test(Opts, 10).

two_two_test(Opts, Threshold) ->
    {ok, Cls} = load_classifier("two-two", Opts),
    ?assertMatch(#result{bestmatch=0, p_ratio=PRatio} when PRatio > Threshold,
                 erlcrm114:classify(Cls, read_doc("alice_frag.txt"))),
    ?assertMatch(#result{bestmatch=1, p_ratio=PRatio} when PRatio < -Threshold,
                 erlcrm114:classify(Cls, read_doc("hound_frag.txt"))),
    ?assertMatch(#result{bestmatch=2, p_ratio=PRatio} when PRatio < -Threshold,
                 erlcrm114:classify(Cls, read_doc("macbeth_frag.txt"))),
    ?assertMatch(#result{bestmatch=3, p_ratio=PRatio} when PRatio > Threshold,
                 erlcrm114:classify(Cls, read_doc("willows_frag.txt"))).

entropy_test() ->
    classic_test([entropy]),
    classic_test([entropy, unique]),
    classic_test([entropy, unique, crosslink]),
    maturity_test([entropy, unique, crosslink]),
    four_class_test([entropy]),
    two_two_test([entropy, unique, crosslink]).

hyperspace_test() ->
    classic_test([hyperspace]),
    classic_test([hyperspace, string]),
    classic_test([hyperspace, unigram]),
    classic_test([hyperspace, {regex, <<"\\w+">>}]),
    maturity_test([hyperspace]),
    four_class_test([hyperspace]),
    two_two_test([hyperspace], 1).

osb_test() ->
    classic_test([osb]),
    classic_test([osb, unique]),
    classic_test([osb, {regex, <<"[a-zA-Z]+">>}]),
    maturity_test([osb]),
    four_class_test([osb]),
    two_two_test([osb]).

pca_test() ->
    classic_test([pca]),
    maturity_test([pca]),
    maturity_test([pca, unique]),
    maturity_test([pca, unigram, unique]).

svm_test() ->
    classic_test([svm]),
    classic_test([svm, unigram]),
    classic_test([svm, unique]),
    classic_test([svm, unigram, unique]),
    maturity_test([svm]).
