Rebar3 gpb plugin
=====

[![Build Status](https://travis-ci.org/lrascao/rebar3_gpb_plugin.svg?branch=master)](https://travis-ci.org/lrascao/rebar3_gpb_plugin)

A rebar3 plugin for automatically compiling .proto files using the gpb protobuf compiler

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {erl_opts, {i, "./_build/default/plugins/gpb/include"}}.

    {plugins, [
        { rebar3_gpb_plugin, "1.3.1" }
    ]}.

Configure gpb options (example below), full list can consulted on [gpb's project page](https://github.com/tomas-abrahamsson/gpb) [gpb_compile:file/2](https://github.com/tomas-abrahamsson/gpb/blob/3.19.0/src/gpb_compile.erl#L66-L93):

    {gpb_opts, [
        {i, "path/to/proto_dir"},
        {module_name_suffix, "_pb"},
        {o_erl, "path/to/out_src"},
        {o_hrl, "path/to/out_include"},
        {strings_as_binaries, true},
        type_specs]}.

The `o_erl` and `o_hrl` values are relative to the app's location.

Add the gpb include path (environment tipically is default):

    {erl_opts, {i, "./_build/<environment>/plugins/gpb/include"}}.

Add a hook to automatically generate modules for your protobuf files and clean them afterwards:

    {provider_hooks, [
        {pre, [
            {compile, {protobuf, compile}}
            {clean, {protobuf, clean}}
        ]}
    ]}.
