-module(sample_proto_test).
-author("hnrkptrsn").

-export([
    create_empty_person/0,
    create_named_person/1]).

% This imports the record for you to use from the generated file
-include("addressbook_pb.hrl").


create_empty_person() ->
    #person{}.

create_named_person(Name) ->
    #person{name = Name}.
