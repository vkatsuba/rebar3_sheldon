%%% @doc Plugin provider for rebar3 rebar3_sheldon.
-module(rebar3_sheldon_prv).

-export([init/1, do/1, format_error/1]).

-ignore_xref([do/1,
              format_error/1,
              {providers, create, 1},
              {rebar_state, add_provider, 2},
              {rebar_state, command_parsed_args, 1}]).

-define(PROVIDER, spellcheck).
-define(DEPS, []).
-define(OPTS, []).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER}, % The 'user friendly' name of the task
                          {module, ?MODULE}, % The module implementation of the task
                          {bare, true},      % The task can be run by the user, always true
                          {deps, ?DEPS},     % The list of dependencies
                          {example, "rebar3 spellcheck"}, % How to use the plugin
                          {opts, ?OPTS},     % list of options understood by the plugin
                          {short_desc, "A spellcheck plugin"},
                          {desc, "Plugin for spellcheck by sheldon "}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Args = parse_opts(State),
    ok = rebar_api:debug("Args: ~p", [Args]),

    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
        end,
    Cwd = rebar_state:dir(State),
    Dirs = [dir_for_app(AppInfo, Cwd) || AppInfo <- Apps],

    IgnoredFiles = get_ignored_files(State),
    Files = get_files(Args, State, Dirs) -- IgnoredFiles,

    rebar_api:debug("Found ~p files: ~p", [length(Files), Files]),

    case spellcheck(Files) of
        [] ->
            {ok, State};
        Warnings -> %% @TODO: sheldon will return warning for TODO word
            {error, io_lib:format("spellcheck detect warning emits: ~p", [Warnings])}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec parse_opts(rebar_state:t()) -> list().
parse_opts(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Args.

-spec dir_for_app(rebar_app_info:t(), file:filename_all()) -> file:filename_all() | [].
dir_for_app(AppInfo, Cwd) ->
    {ok, Dir} =
        rebar_file_utils:path_from_ancestor(
            rebar_app_info:dir(AppInfo), Cwd),
    Dir.

-spec get_files(proplists:proplist(), rebar_state:t(), [file:filename_all() | []]) ->
                   [file:filename_all()].
get_files(Args, State, Dirs) ->
    FilesFromArgs = [Value || {files, Value} <- Args],
    {Patterns, Dirs1} =
        case FilesFromArgs of
            [] ->
                SheldonConfig = rebar_state:get(State, spellcheck, []),
                case proplists:get_value(files, SheldonConfig, undefined) of
                    undefined ->
                        {["include/**/*.[he]rl",
                          "include/**/*.app.src",
                          "src/**/*.[he]rl",
                          "src/**/*.app.src",
                          "test/**/*.[he]rl",
                          "test/**/*.app.src",
                          "{rebar,elvis,sys}.config"],
                         Dirs};
                    Wildcards ->
                        {Wildcards, []}
                end;
            Files ->
                {Files, []}
        end,
    %% Special handling needed for "" (current directory)
    %% so that ignore-lists work in an expected way.
    [File || Pattern <- Patterns, File <- filelib:wildcard(Pattern)]
    ++ [filename:join(Dir, File)
        || Dir <- Dirs1, Dir =/= "", Pattern <- Patterns, File <- filelib:wildcard(Pattern, Dir)].

-spec get_ignored_files(rebar_state:t()) -> [file:filename_all()].
get_ignored_files(State) ->
    FormatConfig = rebar_state:get(State, spellcheck, []),
    Patterns = proplists:get_value(ignore, FormatConfig, []),
    [IgnoredFile || Pat <- Patterns, IgnoredFile <- filelib:wildcard(Pat)].

%% =============================================================================
%% Spellcheck collectors
%% =============================================================================

-spec spellcheck([file:filename(), ...] | []) -> [maps:map(), ...] | [].
spellcheck(Filenames) ->
    spellcheck(Filenames, []).

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
