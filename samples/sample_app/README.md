sample_app
=====

An OTP application showing how to use the rebar3_gpb_plugin in a simple erlang project.

This project is not meant to show any interesting OTP behaviour, just how to compile and include proto files and how to use them as part of your own projects.

This project was created using rebar3:

	rebar3 new app sample_app
	
After which the following files were added or modified

- add the rebar3_gpb_plugin to your rebar.config
- add a directory for your [protocol buffer](https://developers.google.com/protocol-buffers/) definition.
- add a simple protocol buffer definition
- add a module to test your protocol buffer
- compile your project 
- finally call your function

Build
-----

    $ rebar3 compile

	$ rebar3 shell
	
	> sample_proto_test:create_empty_person().
	
	> sample_proto_test:create_named_person(<<"John">>).
	
	

