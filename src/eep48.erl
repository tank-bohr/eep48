-module(eep48).
-export([
    edocc/0,
    edocs/1
]).

-define(TARGET, hackney).

-include_lib("edoc/src/edoc.hrl").

% rr(filename:join(code:lib_dir(edoc, src), "edoc.hrl")).
% rr(filename:join(code:lib_dir(xmerl, include), "xmerl.hrl")).

edocc() ->
    SrcDir = code:lib_dir(?TARGET, src),
    IncludeDir = code:lib_dir(?TARGET, include),
    SrcList = filelib:wildcard(SrcDir ++ "/*.erl"),
    lists:foreach(fun(Src) ->
        Docs = edocs(Src),
        DocsChunkData = term_to_binary(Docs, [compressed]),
        ExtraChunks = [{<<"Docs">>, DocsChunkData}],
        compile:file(Src, [
            debug_info,
            {extra_chunks, ExtraChunks},
            {outdir, source_beam()},
            {i, IncludeDir},
            {i, SrcDir}
        ])
    end, SrcList).

edocs(Src) ->
    Opts = [],
    Env = edoc_lib:get_doc_env(Opts),
    {_Module, Entries} = eep48_extract:source(Src, Env, Opts),
    docs(Entries).

docs(Entries) ->
    #entry{name = module, line = Line, data = Data} = get_entry(module, Entries),
    {
        docs_v1,
        erl_anno:new(Line),
        erlang,
        <<"text/markdown">>,
        #{ <<"en">> => doc_string(Data) },
        #{},
        function_docs(Entries)
    }.

get_entry(Name, [#entry{name = Name} = E | _Es]) -> E;
get_entry(Name, [_ | Es]) -> get_entry(Name, Es).

function_docs(Entries) ->
    lists:filtermap(fun function_doc/1, Entries).

function_doc(#entry{name = {_Name, _Arity}} = Entry) ->
    {true, process_function_entry(Entry)};
function_doc(_) ->
    false.

process_function_entry(#entry{name = {Name, Arity}, line = Line, args = Args, data = Data}) ->
    {
        {function, Name, Arity},
        erl_anno:new(Line),
        signature(Name, Args),
        #{ <<"en">> => doc_string(Data) },
        #{}
    }.

signature(Name, ArgList) ->
    Args = lists:join(", ", [atom_to_list(A) || A <- ArgList]),
    [iolist_to_binary(io_lib:format("~s(~s)", [Name, Args]))].

doc_string(Data) ->
    iolist_to_binary(
        lists:map(fun tag_to_markdown/1, Data)).

tag_to_markdown(#tag{name = doc, data = Xml}) ->
    xmerl:export_simple(Xml, edown_xmerl);
tag_to_markdown(_) ->
    "".

source_beam() ->
    filename:join(code:priv_dir(?MODULE), "ebin").
