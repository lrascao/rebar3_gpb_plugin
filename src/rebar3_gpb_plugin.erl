-module(rebar3_gpb_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar3_gpb_prv_compile:init(State0),
    {ok, State2} = rebar3_gpb_prv_clean:init(State1),
    {ok, State2}.
