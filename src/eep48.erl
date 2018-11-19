-module(eep48).
-export([
    edoc/0,
    to_md/1,
    to_xml/1,
    doc_tag/1,
    file_path/0
]).

-include_lib("edoc/src/edoc.hrl").
% rr(filename:join(code:lib_dir(edoc, src), "edoc.hrl")).
% rr(filename:join(code:lib_dir(xmerl, include), "xmerl.hrl")).

edoc() ->
    Src = file_path(),
    Opts = [],
    Env = edoc_lib:get_doc_env(Opts),
    {_Module, Data} = eep48_extract:source(Src, Env, Opts),
    lists:filtermap(fun entry/1, Data).

entry(#entry{name = {_Name, _Arity}} = Entry) ->
    {true, Entry};
entry(_) ->
    false.

doc_tag(#entry{data = Data}) ->
    [DocTag] = lists:filtermap(fun eep48:doc_tag/1, Data),
    DocTag;
doc_tag(#tag{name = doc, data = Data}) ->
    {true, Data};
doc_tag(_) ->
    false.

to_md(Xml) when is_list(Xml) ->
    io:format("~s~n", [xmerl:export_simple(Xml, edown_xmerl)]);
to_md(Xml) ->
    to_md([Xml]).

to_xml(Xml) when is_list(Xml) ->
    io:format("~s~n", [xmerl:export_simple(Xml, xmerl_xml)]);
to_xml(Xml) ->
    to_xml([Xml]).

file_path() ->
    filename:join(code:priv_dir(?MODULE), "riak_client.erl").
