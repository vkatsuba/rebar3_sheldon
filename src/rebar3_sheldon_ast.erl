-module(rebar3_sheldon_ast).

-export([spellcheck/2]).

%% =============================================================================
%% Spellcheck API
%% =============================================================================

-spec spellcheck([file:filename(), ...] | [], string() | undefined) ->
                    [maps:map(), ...] | [].
spellcheck(Filenames, RegEx) ->
    spellcheck(Filenames, [], RegEx).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec spellcheck([file:filename(), ...], list(), string() | undefined) ->
                    [maps:map(), ...] | [].
spellcheck([], Acc, _) ->
    Acc;
spellcheck([Filename | T], Acc, RegEx) ->
    Comments = collect_comments(Filename, RegEx),
    Strings = collect_strings(Filename, RegEx),
    Result = lists:sort(Comments ++ Strings),
    NewAcc =
        lists:foldl(fun({Line, Type, Str}, Res) ->
                       case sheldon:check(Str) of
                           ok ->
                               Res;
                           Warnings ->
                               [#{file => Filename,
                                  line => Line,
                                  type => Type,
                                  string => Str,
                                  reason => Warnings}
                                | Res]
                       end
                    end,
                    Acc,
                    Result),
    spellcheck(T, NewAcc, RegEx).

-spec collect_strings(string(), string() | undefined) ->
                         [{erl_anno:anno(), binary | string | comment, string()}].
collect_strings(Filename, RegEx) ->
    {ok, Bin} = file:read_file(Filename),
    TrimBin = binary:replace(Bin, [<<"~p">>, <<"~n">>, <<"@">>], <<>>, [global]),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(TrimBin)),
    collect_strings(Tokens, [], RegEx).

-spec collect_strings([term(), ...], list(), string() | undefined) ->
                         [{integer(), string | binary, string()}, ...] | [].
collect_strings([], Acc, _) ->
    Acc;
collect_strings([{'<<', Line}, {string, Line, Str} | T], Acc, RegEx) ->
    case is_ignore(Str, RegEx) of
        true ->
            collect_strings(T, Acc, RegEx);
        false ->
            collect_strings(T, [{Line, binary, Str} | Acc], RegEx)
    end;
collect_strings([{string, Line, Str} | T], Acc, RegEx) ->
    case is_ignore(Str, RegEx) of
        true ->
            collect_strings(T, Acc, RegEx);
        false ->
            collect_strings(T, [{Line, string, Str} | Acc], RegEx)
    end;
collect_strings([_ | T], Acc, RegEx) ->
    collect_strings(T, Acc, RegEx).

-spec collect_comments(file:filename(), string() | undefined) ->
                          [{integer(), comment, string()}, ...] | [].
collect_comments(Filename, RegEx) ->
    collect_comments(erl_comment_scan:file(Filename), [], RegEx).

-spec collect_comments(term(), list(), string() | undefined) ->
                          [{integer(), comment, string()}, ...] | [].
collect_comments([], Acc, _) ->
    Acc;
collect_comments([{Line, _Column, _Indent, Comments} | T], AccC, RegEx) ->
    NewAccC =
        lists:foldl(fun (Comment, [{L, comment, _} | _] = Acc) ->
                            case is_ignore(Comment, RegEx) of
                                true ->
                                    Acc;
                                false ->
                                    [{L + 1, comment, re_replace(Comment)} | Acc]
                            end;
                        (Comment, Acc) ->
                            case is_ignore(Comment, RegEx) of
                                true ->
                                    Acc;
                                false ->
                                    [{Line, comment, re_replace(Comment)} | Acc]
                            end
                    end,
                    [],
                    Comments)
        ++ AccC,
    collect_comments(T, NewAccC, RegEx).

-spec re_replace(string() | binary()) -> string().
re_replace(Str) ->
    re:replace(
        unicode:characters_to_binary(Str), "%|@", "", [global, {return, list}]).

-spec is_ignore(string(), string() | undefined) -> boolean().
is_ignore(_, undefined) ->
    false;
is_ignore(String, RegEx) ->
    re:run(
        unicode:characters_to_binary(String), RegEx)
    /= nomatch.
