-module(rebar3_sheldon_ast).

-export([spellcheck/1]).

%% =============================================================================
%% Spellcheck API
%% =============================================================================

-spec spellcheck([file:filename(), ...] | []) -> [maps:map(), ...] | [].
spellcheck(Filenames) ->
    spellcheck(Filenames, []).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec spellcheck([file:filename(), ...], list()) -> [maps:map(), ...] | [].
spellcheck([], Acc) ->
    Acc;
spellcheck([Filename | T], Acc) ->
    Comments = collect_comments(Filename),
    Strings = collect_strings(Filename),
    Result = lists:sort(Comments ++ Strings),
    NewAcc =
        lists:foldl(fun({Line, Type, Str}, Res) ->
                       case sheldon:check(Str) of
                           ok ->
                               Res;
                           Warnings ->
                               [#{filename => Filename,
                                  line => Line,
                                  type => Type,
                                  string => Str,
                                  reason => Warnings}
                                | Res]
                       end
                    end,
                    Acc,
                    Result),
    spellcheck(T, NewAcc).

-spec collect_strings(string()) ->
                         [{erl_anno:anno(), binary | string | comment, string()}].
collect_strings(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    TrimBin = binary:replace(Bin, [<<"~p">>, <<"~n">>, <<"@">>], <<>>, [global]),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(TrimBin)),
    collect_strings(Tokens, []).

-spec collect_strings([term(), ...], list()) ->
                         [{integer(), string | binary, string()}, ...] | [].
collect_strings([], Acc) ->
    Acc;
collect_strings([{'<<', Line}, {string, Line, Str} | T], Acc) ->
    case string:find(Str, "/*", leading) of
        nomatch ->
            collect_strings(T, [{Line, binary, Str} | Acc]);
        _ ->
            collect_strings(T, Acc)
    end;
collect_strings([{string, Line, Str} | T], Acc) ->
    case string:find(Str, "/*", leading) of
        nomatch ->
            collect_strings(T, [{Line, string, Str} | Acc]);
        _ ->
            collect_strings(T, Acc)
    end;
collect_strings([_ | T], Acc) ->
    collect_strings(T, Acc).

-spec collect_comments(file:filename()) -> [{integer(), comment, string()}, ...] | [].
collect_comments(Filename) ->
    collect_comments(erl_comment_scan:file(Filename), []).

-spec collect_comments(term(), list()) -> [{integer(), comment, string()}, ...] | [].
collect_comments([], Acc) ->
    Acc;
collect_comments([{Line, _Column, _Indent, Comments} | T], AccC) ->
    NewAccC =
        case Comments of
            [Comment] ->
                [{Line, comment, re_replace(Comment)} | AccC];
            Comments ->
                lists:foldl(fun (Comment, [{L, comment, _} | _] = Acc) ->
                                    [{L + 1, comment, re_replace(Comment)} | Acc];
                                (Comment, Acc) ->
                                    [{Line, comment, re_replace(Comment)} | Acc]
                            end,
                            [],
                            Comments)
                ++ AccC %% @TODO: simplify order of multi comments
        end,
    collect_comments(T, NewAccC).

-spec re_replace(string() | binary()) -> string().
re_replace(Str) ->
    re:replace(Str, "%|@", "", [global, {return, list}]).
