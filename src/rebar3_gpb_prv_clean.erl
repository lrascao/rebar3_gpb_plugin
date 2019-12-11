-module(rebar3_gpb_prv_clean).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).
-define(DESC, "Remove compiled Protocol Buffers from apps.").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, protobuf},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 protobuf clean"},
        {short_desc, ?DESC},
        {desc, ""},
        {opts, []}]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
                undefined -> rebar_state:project_apps(State);
                AppInfo   -> [AppInfo]
           end,
    lists:foreach(fun(App) ->
                    rebar3_gpb_compiler:clean(App, State)
                  end, Apps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
