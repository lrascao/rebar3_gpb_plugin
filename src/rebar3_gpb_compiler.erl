-module(rebar3_gpb_compiler).

-export([compile/1,
         clean/1]).

-define(DEFAULT_PROTO_DIR, "proto").
-define(DEFAULT_OUT_ERL_DIR, "src").
-define(DEFAULT_OUT_HRL_DIR, "include").
-define(DEFAULT_MODULE_PREFIX, "").
-define(DEFAULT_MODULE_SUFFIX, "").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t()) -> ok.
compile(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts0} = dict:find(gpb_opts, Opts),
    %% check if non-recursive
    Recursive = proplists:get_value(recursive, GpbOpts0, true),
    SourceDirs = proplists:get_all_values(i, GpbOpts0),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_erl, GpbOpts0,
                                                      ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_hrl, GpbOpts0,
                                                      ?DEFAULT_OUT_HRL_DIR)]),
    rebar_api:debug("making sure that target erl dir ~p exists", [TargetErlDir]),
    ok = ensure_dir(TargetErlDir),
    rebar_api:debug("making sure that target hrl dir ~p exists", [TargetHrlDir]),
    ok = ensure_dir(TargetHrlDir),
    rebar_api:debug("reading proto files from ~p, generating \".erl\" to ~p "
                    "and \".hrl\" to ~p",
      [SourceDirs, TargetErlDir, TargetHrlDir]),
    %% set the full path for the output directories
    %% remove the plugin specific options since gpb will not understand them
    GpbOpts = remove_plugin_opts(
                default_include_opts(AppDir,
                    target_erl_opt(TargetErlDir,
                        target_hrl_opt(TargetHrlDir, GpbOpts0)))),

    Protos = lists:foldl(fun(SourceDir, Acc) ->
                            Acc ++ discover(AppDir, SourceDir, [{recursive, Recursive}])
                         end, [], SourceDirs),
    rebar_api:debug("proto files found: ~p", [Protos]),
    compile(Protos, TargetErlDir, GpbOpts, Protos),
    ok.

-spec clean(rebar_app_info:t()) -> ok.
clean(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts,
                                           ?DEFAULT_MODULE_SUFFIX),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_erl, GpbOpts,
                                                      ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_hrl, GpbOpts,
                                                      ?DEFAULT_OUT_HRL_DIR)]),
    ProtoFiles = find_proto_files(AppDir, GpbOpts),
    GeneratedRootFiles = [filename:rootname(filename:basename(ProtoFile)) ++
                          ModuleNameSuffix || ProtoFile <- ProtoFiles],
    GeneratedErlFiles = [filename:join([TargetErlDir, F ++ ".erl"]) ||
                            F <- GeneratedRootFiles],
    GeneratedHrlFiles = [filename:join([TargetHrlDir, F ++ ".hrl"]) ||
                            F <- GeneratedRootFiles],
    rebar_api:debug("deleting [~p, ~p]",
      [GeneratedErlFiles, GeneratedHrlFiles]),
    rebar_file_utils:delete_each(GeneratedErlFiles ++ GeneratedHrlFiles).

%% ===================================================================
%% Private API
%% ===================================================================
discover(AppDir, SourceDir, Opts) ->
    %% Convert simple extension to proper regex
    SourceExtRe = "^[^._].*\\" ++ ".proto" ++ [$$],

    Recursive = proplists:get_value(recursive, Opts, true),
    %% Find all possible source files
    rebar_utils:find_files(filename:join([AppDir, SourceDir]),
                           SourceExtRe, Recursive).

compile([], _TargetErlDir, _GpbOpts, _Protos) -> ok;
compile([Proto | Rest], TargetErlDir, GpbOpts, Protos) ->
    ModuleNamePrefix = proplists:get_value(module_name_prefix, GpbOpts,
                                           ?DEFAULT_MODULE_PREFIX),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts,
                                           ?DEFAULT_MODULE_SUFFIX),
    Target = target_file(Proto, ModuleNamePrefix, ModuleNameSuffix, TargetErlDir),
    Deps =
      case filelib:last_modified(Target) < filelib:last_modified(Proto) of
          true ->
            ok = compile(Proto, Target, GpbOpts),
            %% now we know that thid proto needed compilation we check
            %% for other protos that might have included this one and ensure that
            %% those are compiled as well
            Deps0 = get_dependencies(Proto, Protos, GpbOpts),
            rebar_api:debug("protos that include ~p: ~p",
              [Proto, Deps0]),
            %% now touch the targets of each of the deps so they get remade again
            lists:foreach(fun(Dep) ->
                            DepTarget = target_file(Dep, ModuleNamePrefix, ModuleNameSuffix,
                                                    TargetErlDir),
                            %% we want to force compilation in this case so we must trick
                            %% the plugin into believing that the proto is more recent than the
                            %% target, this means we need to set the last changed time on the
                            %% target to a time earlier than the proto
                            Seconds = calendar:datetime_to_gregorian_seconds(
                                        filelib:last_modified(Dep)) - 60,
                            _ = file:change_time(DepTarget,
                                                 calendar:gregorian_seconds_to_datetime(Seconds)),
                            rebar_api:debug("touched ~p", [DepTarget])
                          end, Deps0),
            Deps0;
          false ->
              []
      end,
    compile(Rest ++ Deps, TargetErlDir, GpbOpts, Protos).

get_dependencies(Proto, Protos, GpbOpts) ->
    %% go through each of the protos and for each one
    %% check if it included the provided proto, return
    %% the ones tht do
    lists:filtermap(fun(Proto0) ->
                      filter_included_proto(Proto0, Proto, GpbOpts)
                    end, Protos).

filter_included_proto(Source, Dep, GpbOpts) ->
    {ok, Defs, _Warnings} = gpb_compile:file(filename:basename(Source),
                                             GpbOpts ++ [to_proto_defs, return]),
    Imports = lists:map(fun(Import0) ->
                          {ok, Import} = gpb_compile:locate_import(Import0, GpbOpts),
                          filename:absname(Import)
                        end, lists:usort(gpb_parse:fetch_imports(Defs))),
    case lists:member(Dep, Imports) of
      true -> {true, Source};
      false -> false
    end.

target_file(Proto, ModuleNamePrefix, ModuleNameSuffix, TargetErlDir) ->
    Module = filename:basename(Proto, ".proto"),
    filename:join([TargetErlDir, ModuleNamePrefix ++ Module ++ ModuleNameSuffix ++ ".erl"]).

-spec compile(string(), string(), proplists:proplist()) -> ok.
compile(Source, Target, GpbOpts) ->
    rebar_api:debug("compiling ~p to ~p", [Source, Target]),
    rebar_api:debug("opts: ~p", [GpbOpts]),
    case gpb_compile:file(filename:basename(Source), GpbOpts) of
        ok ->
            ok;
        {error, Reason} ->
            ReasonStr = gpb_compile:format_error(Reason),
            rebar_utils:abort("failed to compile ~s: ~s~n", [Source, ReasonStr])
    end.

-spec ensure_dir(filelib:dirname()) -> 'ok' | {error, Reason::file:posix()}.
ensure_dir(OutDir) ->
  %% Make sure that ebin/ exists and is on the path
  case filelib:ensure_dir(filename:join(OutDir, "dummy.beam")) of
    ok -> ok;
    {error, eexist} ->
      rebar_utils:abort("unable to ensure dir ~p, is it maybe a broken symlink?",
        [OutDir]);
    {error, Reason} -> {error, Reason}
  end.

-spec default_include_opts(string(), proplists:proplist()) -> proplists:proplist().
default_include_opts(AppDir, Opts) ->
    Opts ++
    [{i, filename:join(AppDir, Path)} || {i, Path} <- Opts] ++
    [{i, filename:join(AppDir, Path)} || {ipath, Path} <- Opts].

-spec target_erl_opt(string(), proplists:proplist()) -> proplists:proplist().
target_erl_opt(Dir, Opts) ->
    lists:keystore(o_erl, 1, Opts, {o_erl, Dir}).

-spec target_hrl_opt(string(), proplists:proplist()) -> proplists:proplist().
target_hrl_opt(Dir, Opts) ->
    lists:keystore(o_hrl, 1, Opts, {o_hrl, Dir}).

-spec remove_plugin_opts(proplists:proplists()) -> proplists:proplist().
remove_plugin_opts(Opts) ->
    remove_plugin_opts(Opts, [recursive, ipath]).

-spec remove_plugin_opts(proplists:proplist(),
                         [ipath | recursive]) -> proplists:proplist().
remove_plugin_opts(Opts, []) -> Opts;
remove_plugin_opts(Opts0, [OptToRemove | Rest]) ->
    Opts = lists:keydelete(OptToRemove, 1, Opts0),
    remove_plugin_opts(Opts, Rest).

find_proto_files(AppDir, GpbOpts) ->
    lists:foldl(fun(SourceDir, Acc) ->
                Acc ++ rebar_utils:find_files(filename:join(AppDir, SourceDir),
                                   ".*\.proto\$")
              end, [], proplists:get_all_values(i, GpbOpts)).

