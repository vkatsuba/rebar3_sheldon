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

    case rebar3_sheldon_ast:spellcheck(Files) of
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
