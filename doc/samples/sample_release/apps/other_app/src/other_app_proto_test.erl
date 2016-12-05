-module(other_app_proto_test).
-author("hnrkptrsn").

-export([add_id_person/2]).

% This imports the record for you to use from the generated file
-include_lib("sample_release/include/addressbook_pb.hrl").

add_id_person(Person, Id) ->
	Person#person{id=Id}.
