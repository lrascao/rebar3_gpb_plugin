-module(rebar3_gpb_plugin_prv).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, protobuf},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {opts, []},                   % list of options understood by the plugin
            {example, "rebar3 protobuf compile"},
            {short_desc, "Automatically compile .proto files using the gpb compiler"},
            {desc, "Google Protobuf compiler"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
           end,
    lists:foreach(fun(AppInfo) ->
                Opts = rebar_app_info:opts(AppInfo),
                {ok, GpbOpts} = dict:find(gpb_opts, Opts),
                SourceDir = proplists:get_value(i, GpbOpts),
                TargetDir = proplists:get_value(o_erl, GpbOpts),
                ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts),

                rebar_base_compiler:run(Opts, [],
                                        SourceDir, ".proto",
                                        TargetDir, ModuleNameSuffix ++ ".erl",
                                        fun(Source, Target, Config) ->
                                            compile(Source, Target, GpbOpts, Config)
                                        end)
        end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
compile(Source, _Target, GpbOpts, _Config) ->
    gpb_compile:file(filename:basename(Source), GpbOpts).
