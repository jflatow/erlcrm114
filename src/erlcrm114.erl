-module(erlcrm114).

-export([new/0,
         new/1,
         learn/2,
         learn/3,
         classify/2,
         classify/3,
         from_binary/1,
         to_binary/1]).

-include("erlcrm114.hrl").

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

-spec new/0 :: () ->
    {'ok', classifier()} | error().

new() ->
    new([]).

-spec new/1 :: ([flag() | {'regex', pattern()} | {'classes', [classdef()]}]) ->
    {'ok', classifier()} | error().

new(_Options) ->
    ?nif_not_loaded.

-spec learn/2 :: (classifier(), binary()) ->
    {'ok', classifier()} | error().

learn(_Ref, _Text) ->
    ?nif_not_loaded.

-spec learn/3 :: (classifier(), binary(), non_neg_integer()) ->
    {'ok', classifier()} | error().

learn(_Ref, _Text, _ClassNum) ->
    ?nif_not_loaded.

-spec classify/2 :: (classifier(), binary()) ->
    #result{} | error().

classify(_Ref, _Text) ->
    ?nif_not_loaded.

-spec classify/3 :: (classifier(), binary(), ['detail']) ->
    #result{} | error().

classify(_Ref, _Text, _Options) ->
    ?nif_not_loaded.

-spec from_binary/1 :: (binary()) ->
    {'ok', classifier()} | error().

from_binary(_Binary) ->
    ?nif_not_loaded.

-spec to_binary/1 :: (classifier()) ->
    binary() | error().

to_binary(_Ref) ->
    ?nif_not_loaded.
