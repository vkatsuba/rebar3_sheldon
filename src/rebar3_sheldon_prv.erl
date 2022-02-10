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
-define(OPTS,
        [{files, $f, "files", string, "List of files for spellchecker"},
         {ignore, $i, "ignore", string, "List of ignore files for spellchecker"},
         {ignore_regex, $r, "ignore_regex", string, "Regular exemptions for ignore lines"},
         {default_dictionary, $d, "default_dictionary", string, "Set default dictionary"},
         {additional_dictionaries,
          $a,
          "additional_dictionaries",
          string,
          "List of additional dictionaries"}]).

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
    Config = rebar_state:get(State, spellcheck, []),
    SpellcheckConfig = prepare_config(Args, Config),

    ok = rebar_api:debug("Args: ~p", [Args]),

    ok = sheldon_start(SpellcheckConfig),

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
    Files = get_files(Dirs, SpellcheckConfig) -- IgnoredFiles,

    rebar_api:debug("Found ~p files: ~p", [length(Files), Files]),

    IgnoreRegEx = get_ignore_regex(SpellcheckConfig),

    rebar_api:debug("Ignore regular expression: ~p", [IgnoreRegEx]),

    case rebar3_sheldon_ast:spellcheck(Files, IgnoreRegEx) of
        [] ->
            {ok, State};
        Warnings -> %% @TODO: sheldon will return warning for TODO word
            [#{reason := #{bazinga := SheldonMsg}} | _] = Warnings,
            {error, format_results({SheldonMsg, Warnings})}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~ts", [Reason]).

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

-spec get_files([file:filename_all() | []], [{_, _}]) -> [file:filename_all()].
get_files(Dirs, SpellcheckConfig) ->
    {Patterns, Dirs1} =
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
        end,
    %% Special handling needed for "" (current directory)
    %% so that ignore-lists work in an expected way.
    [File || Pattern <- Patterns, File <- filelib:wildcard(Pattern)]
    ++ [filename:join(Dir, File)
        || Dir <- Dirs1, Dir =/= "", Pattern <- Patterns, File <- filelib:wildcard(Pattern, Dir)].

-spec get_ignored_files([{_, _}]) -> [file:filename_all()].
get_ignored_files(SpellcheckConfig) ->
    Patterns = proplists:get_value(ignore, SpellcheckConfig, []),
    [IgnoredFile || Pat <- Patterns, IgnoredFile <- filelib:wildcard(Pat)].

-spec get_ignore_regex([{_, _}]) -> [string()].
get_ignore_regex(SpellcheckConfig) ->
    proplists:get_value(ignore_regex, SpellcheckConfig, undefined).

%% =============================================================================
%% Error formatter
%% =============================================================================

-spec format_results({binary(), [maps:map()]}) -> string().
format_results({SheldonMsg, Results}) ->
    lists:foldr(fun(Result, Acc) -> [Acc, format_result(Result)] end,
                binary_to_list(SheldonMsg) ++ "\n",
                Results).

-spec format_result(maps:map()) -> io:data().
format_result(#{reason := #{misspelled_words := Misspelled}} = Data) ->
    format_sheldon(Misspelled, Data, []).

-spec format_text(string(), list()) -> string().
format_text(Text, Args) ->
    Formatted = io_lib:format(Text, Args),
    Formatted.

-spec format_sheldon(list(), maps:map(), list()) -> string().
format_sheldon([], _, Acc) ->
    Acc;
format_sheldon([#{candidates := [], word := Word} | T],
               #{file := File,
                 line := Line,
                 type := Type} =
                   Data,
               Acc) ->
    NewAcc =
        [Acc,
         format_text("~ts:~tp: The word ~ts in ~ts is unknown.", [File, Line, to_bin(Word), Type]),
         $\n],
    format_sheldon(T, Data, NewAcc);
format_sheldon([#{candidates := Candidates, word := Word} | T],
               #{file := File,
                 line := Line,
                 type := Type} =
                   Data,
               Acc) ->
    FormatCandidates = format_sheldon_candidates(Candidates, []),
    NewAcc =
        [Acc,
         format_text("~ts:~tp: The word ~ts in ~ts is unknown. Maybe you wanted to use ~ts?",
                     [File, Line, to_bin(Word), Type, FormatCandidates]),
         $\n],
    format_sheldon(T, Data, NewAcc).

-spec format_sheldon_candidates([any()], [[[any()] | char()]]) -> [[[any()] | char()]].
format_sheldon_candidates([], Acc) ->
    Acc;
format_sheldon_candidates([Candidate], Acc) ->
    [Acc, format_text("~ts", [Candidate])];
format_sheldon_candidates([Candidate | T], Acc) ->
    format_sheldon_candidates(T, [Acc, format_text("~ts or ", [Candidate])]).

-spec to_bin(binary() | list()) -> binary().
to_bin([_ | _] = X) ->
    list_to_binary(X);
to_bin(X) ->
    X.

%% =============================================================================
%% Start sheldon
%% =============================================================================

-spec sheldon_start([{_, _}]) -> ok.
sheldon_start(Config) ->
    Dictionary = proplists:get_value(default_dictionary, Config, undefined),
    AdditionalDictionaries = proplists:get_value(additional_dictionaries, Config, []),
    ok = set_sheldon_config(default_dictionary, Dictionary),
    ok = set_sheldon_config(additional_dictionaries, AdditionalDictionaries),
    ok = sheldon:start(),
    ok.

-spec set_sheldon_config(atom(), list() | undefined) -> ok.
set_sheldon_config(_, undefined) ->
    ok;
set_sheldon_config(Key, Value) ->
    application:set_env(sheldon, Key, Value).

-spec prepare_config([{_, _}], [{_, _}]) -> [{_, _}].
prepare_config(Args0, Config0) ->
    Config = maps:from_list(Config0),
    Args =
        lists:foldl(fun ({K, V}, Acc)
                            when K =:= files; K =:= ignore; K =:= additional_dictionaries ->
                            Acc#{K => string:tokens(V, ", ")};
                        ({K, V}, Acc) ->
                            Acc#{K => V}
                    end,
                    #{},
                    Args0),
    All = maps:merge(Config, Args),
    maps:to_list(All).
