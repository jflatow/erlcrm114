-module(erlcrm114).

-export([new/0,
         new/1,
         learn/2,
         classify/2]).

-on_load(load/0).

-define(nif_not_loaded, erlang:nif_error({nif_not_loaded, module, ?MODULE, line, ?LINE})).

load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new() ->
    new([]).

new(_Options) ->
    ?nif_not_loaded.

learn(_Ref, _Text) ->
    ?nif_not_loaded.

classify(_Ref, _Text) ->
    ?nif_not_loaded.
