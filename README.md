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

    {plugins, [
        { rebar3_gpb_plugin, ".*",
            {git, "git@github.com:lrascao/rebar3_gpb_plugin.git", {tag, "1.0.1"}}}
    ]}.

Configure gpb options (example below), full list can consulted on [gpb's project page](https://github.com/tomas-abrahamsson/gpb):

    {gpb_opts, [
        {i, "path/to/proto_dir"},
        {module_name_suffix, "_pb"},
        {o_erl, "path/to/out_src"},
        {o_hrl, "path/to/out_include"},
        {strings_as_binaries, true},
        type_specs]}.

Add a hook to automatically generate modules for your protobuf files:

    {provider_hooks, [
        {pre, [
            {compile, {protobuf, compile}}
        ]}
    ]}.
