-module(rebar3_gpb_compiler).

-export([compile/1,
         clean/1]).

-define(DEFAULT_PROTO_DIR, "proto").
-define(DEFAULT_OUT_ERL_DIR, "src").
-define(DEFAULT_OUT_HRL_DIR, "include").
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
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts0, ?DEFAULT_MODULE_SUFFIX),
    SourceDir = filename:join([AppDir,
                               proplists:get_value(i, GpbOpts0,
                                                   ?DEFAULT_PROTO_DIR)]),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_erl, GpbOpts0,
                                                      ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_hrl, GpbOpts0,
                                                      ?DEFAULT_OUT_HRL_DIR)]),
    rebar_api:debug("reading proto files from ~p, generating \"~s.erl\" to ~p and \"~s.hrl\" to ~p",
      [SourceDir, ModuleNameSuffix, TargetErlDir, ModuleNameSuffix, TargetHrlDir]),
    rebar_api:debug("making sure that target erl dir ~p exists", [TargetErlDir]),
    ok = ensure_dir(TargetErlDir),
    rebar_api:debug("making sure that target hrl dir ~p exists", [TargetHrlDir]),
    ok = ensure_dir(TargetHrlDir),
    %% set the full path for the output directories
    GpbOpts = default_include_opts(SourceDir,
                    target_erl_opt(TargetErlDir,
                        target_hrl_opt(TargetHrlDir, GpbOpts0))),
    rebar_base_compiler:run(Opts, [],
                            SourceDir, ".proto",
                            TargetErlDir, ModuleNameSuffix ++ ".erl",
                            fun(Source, Target, Config) ->
                                compile(Source, Target, GpbOpts, Config)
                            end).

-spec clean(rebar_app_info:t()) -> ok.
clean(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    ModuleNameSuffix = proplists:get_value(module_name_suffix, GpbOpts, ?DEFAULT_MODULE_SUFFIX),
    SourceDir = filename:join([AppDir,
                               proplists:get_value(i, GpbOpts, ?DEFAULT_PROTO_DIR)]),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_erl, GpbOpts,
                                                      ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_hrl, GpbOpts,
                                                      ?DEFAULT_OUT_HRL_DIR)]),
    ProtoFiles = rebar_utils:find_files(SourceDir, ".*\.proto\$"),
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
-spec compile(string(), string(), proplists:proplist(), term()) -> ok.
compile(Source, _Target, GpbOpts, _Config) ->
    rebar_api:debug("gpb opts: ~p", [GpbOpts]),
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
default_include_opts(SourceDir, Opts) ->
    Opts ++ [{i, SourceDir}].

-spec target_erl_opt(string(), proplists:proplist()) -> proplists:proplist().
target_erl_opt(Dir, Opts) ->
    lists:keystore(o_erl, 1, Opts, {o_erl, Dir}).

-spec target_hrl_opt(string(), proplists:proplist()) -> proplists:proplist().
target_hrl_opt(Dir, Opts) ->
    lists:keystore(o_hrl, 1, Opts, {o_hrl, Dir}).
