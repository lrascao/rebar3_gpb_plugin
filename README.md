Rebar3 gpb plugin
=====

[![Build Status](https://github.com/lrascao/rebar3_gpb_plugin/workflows/build/badge.svg)](https://github.com/lrascao/rebar3_gpb_plugin)

A rebar3 plugin for automatically compiling .proto files using the gpb protobuf compiler

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
{erl_opts, [{i, "./_build/default/plugins/gpb/include"}]}.

{plugins, [
    { rebar3_gpb_plugin, "2.19.0" }
]}.
```

Configure gpb options (example below), full list can consulted on [gpb's project page](https://github.com/tomas-abrahamsson/gpb) [gpb_compile:file/2](https://hexdocs.pm/gpb/gpb_compile.html#file-2):

```erlang
{gpb_opts, [
    {i, "path/to/proto_dir"} | {i, {deps, "relative/path/from/deps/to/proto_dir"}},
    {f, ["desired_proto_file1.proto"]},
    {module_name_suffix, "_pb"},
    %{o, "path/to/out_dir"},    %% both .erl and .hrl are generated here
    {o_erl, "path/to/out_src"},
    {o_hrl, "path/to/out_include"},
    {strings_as_binaries, true},
    type_specs]}.
```

The `i`, `o_erl` and `o_hrl` option values are relative to the app's location.
Default values are:
* `{i, "proto"}`
* `{o_erl, "src"}`
* `{o_hrl, "include"}`

Plugin specific options (can be used together the gpb ones):

```erlang
{gpb_opts, [
    {recursive, boolean()},
    {ipath, "path/to/another/proto_dir"} | {ipath, {deps, "path/from/deps/dir/to/another/proto/dir"}}
]}.
```

* `{recursive, boolean()}` - look recursively through the provided folders
  to look for .proto files (default is true)
* `{ipath, "path/to/another/proto_dir"}` - paths that are to be added to gpb's
  include path but not searched for .proto files (useful for importing .proto
  files from other .proto).
* `{i, {deps, "relative/path/from/deps/to/proto_dir"}}` - allows you to compile
  proto files that were declared and fetched as app dependencies, the [rebar_raw_resource](https://github.com/basho/rebar_raw_resource)
  plugin is a good fit for this use case.
* `{ipath, {deps, "relative/path/from/deps/to/proto_dir"}}` - allows you to specify
  proto include paths in the same fashion as the `{i, {deps, _}}` directive.
* `{f, ["file1.proto", "file2.proto"]} - allows you to specify a subset of all proto files found.
Add the gpb include path (environment tipically is default):

    {erl_opts, [{i, "./_build/<environment>/plugins/gpb/include"}]}.

Add a hook to automatically generate modules for your protobuf files and clean them afterwards:

```erlang
{provider_hooks, [
    {pre, [
        {compile, {protobuf, compile}},
        {clean, {protobuf, clean}}
    ]}
]}.
```

Usage with umbrella projects
----------------------------

When using the gpb plugin with umbrella projects (aka having a `apps/<project>` structure) the `gpb_opts` and `provider_hooks` should **not** be in the top level `rebar.config`! Doing so would lead to undesired behavior and problems like code files not being compiled.

The solution is to create  `apps/<project>/rebar.config` and specify `gpb_opts` and `provider_hooks` in this instead of the top level config. An example config would look like this:


```erlang
%% -*- erlang -*-

%%-------------------------------------------------------------------
%% GPB
%%-------------------------------------------------------------------

{gpb_opts, [
    {i, "proto"},
    {module_name_suffix, "_pb"},
    {o_erl, "src"},
    {o_hrl, "include"},
    {strings_as_binaries, true},
    type_specs]}.
{provider_hooks,
 [{pre, [
         {compile, {protobuf, compile}},
         {clean, {protobuf, clean}}
        ]}
 ]}.
 ```

More examples
-------------

Complete usage examples are located in [doc/samples](/doc/samples).
