sample_release
=====

An OTP application showing how to use the rebar3_gpb_plugin in a release structured (umbrella) Erlang project.

The basic idea is that we want to generate the protocol buffer files in one app and reuse the definitions across the complete set of apps.

This project was created using rebar3:

	$ rebar3 new release sample_release
	$ cd sample_release/apps
	$ rebar3 new app other_app

	$ rebar3 --version
	rebar 3.3.2 on Erlang/OTP 19 Erts 8.1
	
After which the following files were added or modified:

	- add the rebar3_gpb_plugin to your top rebar.config.
	
for the app named sample_release add the following:
	
	- add a directory for your [protocol buffer](https://developers.google.com/protocol-buffers/) definition.
	- add an empty include directory for the generated protocol buffer definitions.
	- add a pattern to .gitignore generated files from git.
	- add a simple protocol buffer definition. (addressbook.proto)
	- add a module to test your protocol buffer.
	- add a rebar.config for the sample_release app to compile to protocol buffer definitions.

finally:

	- compile your project.
	- finally call your function.

for the app named other_app add the following:

	- a module to test using the protocol buffer definitions from the sample_release app

Build
-----

	$ rebar3 compile

	$ rebar3 shell

	1> P = sample_proto_test:create_empty_person().
	{person,undefined,undefined,undefined}


	2> P2 = sample_proto_test:create_named_person(<<"John">>).
	{person,<<"John">>,undefined,undefined}

	3> P3 = other_app_proto_test:add_id_person(P2, 7).
	{person,<<"John">>,7,undefined}
		
	
