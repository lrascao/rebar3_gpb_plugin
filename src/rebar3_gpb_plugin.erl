-module('rebar3_gpb_plugin').

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_gpb_plugin_prv:init(State).
