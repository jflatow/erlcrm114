-ifndef(erlcrm114).
-define(erlcrm114, true).

-type classifier() :: binary().
-type direction()  :: 'success' | 'failure'.
-type error()      :: {'error', term()}.
-type flag()       :: atom().
-type classdef()   :: string() | {string(), direction()}.
-type pattern()    :: binary().

-record(detail, {
          p_ratio    :: float(),                        % r.class[i].pR
          p_success  :: float(),                        % r.class[i].prob
          name       :: string(),                       % r.class[i].name
          nhits      :: non_neg_integer(),              % r.class[i].hits
          nfeatures  :: non_neg_integer(),              % r.class[i].features
          ndocuments :: non_neg_integer(),              % r.class[i].documents
          direction  :: direction()                     % r.class[i].success
         }).

-record(result, {
          p_ratio   :: float(),                         % r.overall_pR
          p_success :: float(),                         % r.tsprob
          bestmatch :: non_neg_integer(),               % r.bestmatch_index
          nfeatures :: non_neg_integer(),               % r.unk_features
          classes   :: non_neg_integer() | [#detail{}]  % r.how_many_classes | r.class[]
         }).

-endif. % -ifdef(erlcrm114).
