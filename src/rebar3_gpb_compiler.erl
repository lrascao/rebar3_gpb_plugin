-module(rebar3_gpb_compiler).

-export([compile/2,
         clean/2]).

-define(DEFAULT_PROTO_DIR, "proto").
-define(DEFAULT_OUT_ERL_DIR, "src").
-define(DEFAULT_OUT_HRL_DIR, "include").

-define(f(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t(),
              rebar_state:t()) -> ok.
compile(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    DepsDir = rebar_dir:deps_dir(State),
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
    %% search for .proto files
    FoundProtos = lists:foldl(fun({deps, SourceDir}, Acc) ->
                                      Acc ++ discover(DepsDir, SourceDir, [{recursive, Recursive}]);
                                 (SourceDir, Acc) ->
                                      Acc ++ discover(AppDir, SourceDir, [{recursive, Recursive}])
                              end, [], SourceDirs),
    rebar_api:debug("proto files found~s: ~p",
                    [case Recursive of true -> " recursively"; false -> "" end, FoundProtos]),

    Protos = case proplists:get_value(f, GpbOpts0) of
                 undefined ->
                     FoundProtos;
                 WantedProtos ->
                     rebar_api:debug("Applying filter: ~p", [WantedProtos]),
                     filter_unwanted_protos(WantedProtos, FoundProtos)
             end,
    rebar_api:debug("Filtered protos: ~p", [Protos]),
    %% set the full path for the output directories
    %% add to include path dir locations of the protos
    %% remove the plugin specific options since gpb will not understand them
    GpbOpts = remove_plugin_opts(
                proto_include_paths(AppDir, Protos,
                  default_include_opts(AppDir, DepsDir,
                      target_erl_opt(TargetErlDir,
                          target_hrl_opt(TargetHrlDir, GpbOpts0))))),

    compile(Protos, TargetErlDir, GpbOpts, Protos),
    ok.

-spec clean(rebar_app_info:t(),
            rebar_state:t()) -> ok.
clean(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    DepsDir = rebar_dir:deps_dir(State),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts} = dict:find(gpb_opts, Opts),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_erl, GpbOpts,
                                                      ?DEFAULT_OUT_ERL_DIR)]),
    TargetHrlDir = filename:join([AppOutDir,
                                  proplists:get_value(o_hrl, GpbOpts,
                                                      ?DEFAULT_OUT_HRL_DIR)]),
    ProtoFiles = find_proto_files(AppDir, DepsDir, GpbOpts),
    rebar_api:debug("found proto files: ~p", [ProtoFiles]),
    GeneratedRootFiles =
        lists:usort(
          [begin
               ProtoGpbOpts = get_proto_gpb_opts(ProtoFile, GpbOpts),
               filename:rootname(get_target(ProtoFile, ProtoGpbOpts))
           end
           || ProtoFile <- ProtoFiles]),
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
    Target = get_target(Proto, GpbOpts),
    ProtoGpbOpts = get_proto_gpb_opts(Proto, GpbOpts),
    Deps =
      case filelib:last_modified(Target) < filelib:last_modified(Proto) of
          true ->
            ok = compile(Proto, Target, ProtoGpbOpts),
            %% now we know that this proto needed compilation we check
            %% for other protos that might have included this one and ensure that
            %% those are compiled as well
            Deps0 = get_dependencies(Proto, Protos, ProtoGpbOpts),
            rebar_api:debug("protos that include ~p: ~p",
              [Proto, Deps0]),
            %% now touch the targets of each of the deps so they get remade again
            lists:foreach(fun(Dep) ->
                            DepGpbOpts = get_proto_gpb_opts(Dep, GpbOpts),
                            DepTarget = get_target(Dep, DepGpbOpts),
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

get_target(Proto, GpbOpts) ->
    InputsOutputs = gpb_compile:list_io(Proto, GpbOpts),
    {erl_output, Erl} = lists:keyfind(erl_output, 1, InputsOutputs),
    Erl.

get_dependencies(Proto, Protos, GpbOpts) ->
    %% go through each of the protos and for each one
    %% check if it included the provided proto, return
    %% the ones that do
    lists:filtermap(fun(Proto0) ->
                      filter_included_proto(Proto0, Proto, GpbOpts)
                    end, Protos).

filter_included_proto(Source, Dep, GpbOpts) ->
    InputsOutputs = gpb_compile:list_io(Source, GpbOpts),
    {sources, SourceAndImports} = lists:keyfind(sources, 1, InputsOutputs),
    Imports = tl(SourceAndImports),
    case lists:member(Dep, Imports) of
        true -> {true, Source};
        false -> false
    end.


find_first_match(WantedProto, []) ->
    rebar_api:abort("Filtered proto file not found in path: ~p", [WantedProto]);

find_first_match(WantedProto, [Head | RemainingProtos]) ->
    case is_wanted_proto(WantedProto, Head)of
        true ->
            Head;
        false ->
            find_first_match(WantedProto, RemainingProtos)
    end.


is_wanted_proto(WantedProto, ProtoPath) ->
    case string:sub_string(ProtoPath, length(ProtoPath) - length(WantedProto) + 1) of
        WantedProto -> true;
        _Other -> false
    end.

filter_unwanted_protos(WantedProtos, AllProtos) ->
    [find_first_match(WantedProto, AllProtos) || WantedProto <- WantedProtos].



-spec compile(string(), string(), proplists:proplist()) -> ok.
compile(Source, Target, GpbOpts) ->
    {ProtoGpbOpts, Info} = get_proto_gpb_opts_with_info(Source, GpbOpts),
    OriginText = [?f(" (~s)", [Info]) || Info =/= ""],
    rebar_api:debug("compiling ~p to ~p", [Source, Target]),
    rebar_api:debug("opts~s:~n  ~p", [OriginText, ProtoGpbOpts]),
    case gpb_compile:file(filename:basename(Source), ProtoGpbOpts) of
        ok ->
            ok;
        {ok, []} ->
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

-spec default_include_opts(string(), string(), proplists:proplist()) -> proplists:proplist().
default_include_opts(AppDir, DepsDir, Opts) ->
    lists:map(fun({i, {deps, Path}}) ->
                    {i, filename:join(DepsDir, Path)};
                 ({i, Path}) ->
                    {i, filename:join(AppDir, Path)};
                 ({ipath, {deps, Path}}) ->
                    {i, filename:join(DepsDir, Path)};
                 ({ipath, Path}) ->
                    {i, filename:join(AppDir, Path)};
                 (Opt) -> Opt
              end, Opts).

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

find_proto_files(AppDir, DepsDir, GpbOpts) ->
    lists:foldl(fun({deps, SourceDir}, Acc) ->
                    Acc ++ rebar_utils:find_files(
                             filename:join(DepsDir, SourceDir),
                                           ".*\.proto\$");
                   (SourceDir, Acc) ->
                    Acc ++ rebar_utils:find_files(
                             filename:join(AppDir, SourceDir),
                                           ".*\.proto\$")
                end,
                [], proplists:get_all_values(i, GpbOpts)).

proto_include_paths(_AppDir, [], Opts) -> Opts;
proto_include_paths(AppDir, [Proto | Protos], Opts) ->
  ProtoDir = filename:join([AppDir, filename:dirname(Proto)]),
  proto_include_paths(AppDir, Protos, Opts ++ [{i, ProtoDir}]).

%% Allow to override options per .proto. If no special override is found,
%% the GpbOpts is used.  The idea is that GpbOpts can look like this:
%%
%%  [...
%%   Common gpb opts
%%   These will be used if no per-proto override is specified
%%   NB: Currently output-dir options cannot be overridden
%%       ie, any common {o,Dir}, {o_erl,Dir} or {o_hrl,Dir} are used
%%   ...
%%   {per_proto_override,
%%    [{"dir1/a.proto", [{module_name_prefix, "dir1_"}]},
%%     {"dir2/a.proto", [{module_name_prefix, "dir2_"}]},
%%     {"dir2/b.proto", {replace,    % <-- means to override all common options
%%                       [{verify, false}, ...]}},
%%     {"dir2/c.proto", {merge,    % <-- means to prepend to common options,
%%                                 %     this is the default,
%%                                 %     as in the a.proto examples above
%%                       [{verify, false}, ...]}}]}]
%%
get_proto_gpb_opts(ProtoFile, GpbOpts) ->
    {GpbOpts2, _Info} = get_proto_gpb_opts_with_info(ProtoFile, GpbOpts),
    GpbOpts2.

get_proto_gpb_opts_with_info(ProtoFile, GpbOpts) ->
    case proplists:get_value(per_proto_override, GpbOpts) of
        undefined ->
            {GpbOpts, "no overrides"};
        ProtoSpecificGpbOpts ->
            case locate_proto_specific_opts(ProtoFile, ProtoSpecificGpbOpts) of
                {found, {merge, ProtoGpbOpts}, MatchingKey} ->
                    Info = ?f("options from ~p were prepended", [MatchingKey]),
                    {ProtoGpbOpts ++ GpbOpts, Info};
                {found, {replace, ProtoGpbOpts}, MatchingKey} ->
                    Info = ?f("options from ~p were used", [MatchingKey]),
                    {ProtoGpbOpts, Info};
                not_found ->
                    {GpbOpts, ""}
            end
    end.

locate_proto_specific_opts(ProtoFile, [{ProtoName, PerProtoOpts} | Rest]) ->
    case tail_of_path_matches(ProtoFile, ProtoName) of
        true ->
            case PerProtoOpts of
                {merge, MoreOpts} ->
                    rebar_api:debug("Per-proto entry for ~p matches, "
                                    "indicating merge", [ProtoName]),
                    {found, {merge, MoreOpts}, ProtoName};
                {replace, NewOpts} ->
                    rebar_api:debug("Per-proto entry for ~p matches, "
                                    "indicating replace", [ProtoName]),
                    {found, {replace, NewOpts}, ProtoName};
                _ when is_list(PerProtoOpts) ->
                    rebar_api:debug("Per-proto entry for ~p matches, "
                                    "will merge opts", [ProtoName]),
                    {found, {merge, PerProtoOpts}, ProtoName};
                _ ->
                    rebar_api:error("Invalid entry for ~p, expecting"
                                    "Opts, {merge, Opts} or {replace, Opts}. "
                                    "Ignoring this.",
                                    [ProtoName]),
                    not_found
            end;
        false ->
            locate_proto_specific_opts(ProtoName, Rest)
    end;
locate_proto_specific_opts(_ProtoFile, []) ->
    not_found.

%% Check whether tails matches. Example
%%   tail_of_path_matches("/home/user/a/b/c/d.proto", "d.proto") -> true
%%   tail_of_path_matches("/home/user/a/b/c/d.proto", "c/d.proto") -> true
%%   tail_of_path_matches("/home/user/a/b/c/d.proto", "b/c/d.proto") -> true
%%   tail_of_path_matches("/home/user/a/b/c/d.proto", "x.proto") -> false
%%   tail_of_path_matches("/home/user/a/b/c/d.proto", "c/x.proto") -> false
tail_of_path_matches(ProtoFile, ProtoOptKey) ->
    tail_matches(lists:reverse(filename:split(ProtoFile)),
                     lists:reverse(filename:split(ProtoOptKey))).

tail_matches([Same | Rest1], [Same | Rest2]) -> tail_matches(Rest1, Rest2);
tail_matches(_, []) -> true;
tail_matches(_, _) -> false.
