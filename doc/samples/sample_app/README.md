sample_app
=====

An OTP application showing how to use the rebar3_gpb_plugin in a simple erlang project.

This project is not meant to show any interesting OTP behaviour, just how to compile and include proto files and how to use them as part of your own projects.

This project was created using rebar3:

	$ rebar3 new app sample_app
	
	$ rebar3 --version
	rebar 3.3.2 on Erlang/OTP 19 Erts 8.1
	
After which the following files were added or modified

	- add the rebar3_gpb_plugin to your rebar.config.
	- add a directory for your [protocol buffer](https://developers.google.com/protocol-buffers/) definition.
	- add an empty include directory for the generated protocol buffer definitions.
	- add a pattern to .gitignore generated files from git.
	- add a simple protocol buffer definition. (addressbook.proto)
	- add a module to test your protocol buffer.
	- compile your project.
	- finally call your function.

Build
-----

    $ rebar3 compile

	$ rebar3 shell
	
	1> P = sample_proto_test:create_empty_person().
	{person,undefined,undefined,undefined}
	
	
	2> P2 = sample_proto_test:create_named_person(<<"John">>).
	{person,<<"John">>,undefined,undefined}
	
	

