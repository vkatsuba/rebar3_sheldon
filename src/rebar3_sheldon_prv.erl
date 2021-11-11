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
    SpellcheckConfig = rebar_state:get(State, spellcheck, []),
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

    IgnoredFiles = get_ignored_files(SpellcheckConfig),
    Files = get_files(Args, Dirs, SpellcheckConfig) -- IgnoredFiles,

    rebar_api:debug("Found ~p files: ~p", [length(Files), Files]),

    IgnoreRegEx = get_ignore_regex(SpellcheckConfig),

    rebar_api:debug("Ignore regular expression: ~p", [IgnoreRegEx]),

    case rebar3_sheldon_ast:spellcheck(Files, IgnoreRegEx) of
        [] ->
            {ok, State};
        Warnings -> %% @TODO: sheldon will return warning for TODO word
            [#{reason := #{bazinga := SheldonMsg}} | _] = Warnings,
            {error, format_results({unicode:characters_to_list(SheldonMsg), Warnings})}
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

-spec get_files(proplists:proplist(), [file:filename_all() | []], list()) ->
                   [file:filename_all()].
get_files(Args, Dirs, SpellcheckConfig) ->
    FilesFromArgs = [Value || {files, Value} <- Args],
    {Patterns, Dirs1} =
        case FilesFromArgs of
            [] ->
                case proplists:get_value(files, SpellcheckConfig, undefined) of
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

-spec get_ignored_files(list()) -> [file:filename_all()].
get_ignored_files(SpellcheckConfig) ->
    Patterns = proplists:get_value(ignore, SpellcheckConfig, []),
    [IgnoredFile || Pat <- Patterns, IgnoredFile <- filelib:wildcard(Pat)].

-spec get_ignore_regex(list()) -> [string()].
get_ignore_regex(SpellcheckConfig) ->
    proplists:get_value(ignore_regex, SpellcheckConfig, undefined).

%% =============================================================================
%% Error formatter
%% =============================================================================

-spec format_results({string(), [maps:map()]}) -> string().
format_results({SheldonMsg, Results}) ->
    lists:foldr(fun(Result, Acc) -> [Acc, format_result(Result), $\n] end,
                SheldonMsg ++ ":\n",
                Results).

-spec format_result(maps:map()) -> string().
format_result(#{file := File,
                line := Line,
                string := Msg,
                type := Type}) ->
    format_text("~ts:~tp: ~ts: ~ts", [File, Line, Type, Msg]).

-spec format_text(string(), list()) -> string().
format_text(Text, Args) ->
    Formatted = io_lib:format(Text, Args),
    unicode:characters_to_list(Formatted).
