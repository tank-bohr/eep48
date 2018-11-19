-module(eep48).
-export([
    file_path/0,
    edoc/0,
    edoc/1
]).

-include_lib("edoc/src/edoc.hrl").
% rr(filename:join(code:lib_dir(edoc, src), "edoc.hrl")).
% rr(filename:join(code:lib_dir(xmerl, include), "xmerl.hrl")).

file_path() ->
    filename:join(code:priv_dir(?MODULE), "riak_client.erl").

edoc() ->
    edoc(file_path()).

edoc(Src) ->
    Opts = [],
    Env = edoc_lib:get_doc_env(Opts),
    {_Module, Data} = eep48_extract:source(Src, Env, Opts),
    entries(Data).

entries(Data) ->
    lists:filtermap(fun entry/1, Data).

entry(#entry{name = {_Name, _Arity}} = Entry) ->
    {true, process_entry(Entry)};
entry(_) ->
    false.

process_entry(#entry{name = {Name, Arity}, line = Line, args = Args, data = Data}) ->
    {
        {function, Name, Arity},
        Line,
        signature(Name, Args),
        #{ <<"en">> => doc_string(Data) },
        #{}
    }.

signature(Name, ArgList) ->
    Args = lists:join(", ", [atom_to_list(A) || A <- ArgList]),
    iolist_to_binary(io_lib:format("~s(~s)", [Name, Args])).

doc_string(Data) ->
    iolist_to_binary(
        lists:map(fun tag_to_markdown/1, Data)).

tag_to_markdown(#tag{name = doc, data = Xml}) ->
    xmerl:export_simple(Xml, edown_xmerl);
tag_to_markdown(_) ->
    "".
