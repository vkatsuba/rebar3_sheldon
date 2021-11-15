-module(rebar3_sheldon_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl"). % Assertion macros for convenience

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_app/1, no_good_files/1, emits_warnings/1, ignore_regex/1, unicode/1,
         custom_dictionaries/1, custom_dictionaries_error/1]).

-spec all() -> [ct_suite:ct_test_def(), ...].
all() ->
    [test_app,
     no_good_files,
     emits_warnings,
     ignore_regex,
     unicode,
     custom_dictionaries_error,
     custom_dictionaries].

-spec groups() -> [ct_suite:ct_group_def(), ...].
groups() ->
    [].

-spec init_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) ->
                           ct_suite:ct_config() | {fail, term()} | {skip, term()}.
init_per_testcase(_Name, Config) ->
    Config.

-spec end_per_testcase(ct_suite:ct_testname(), ct_suite:ct_config()) ->
                          term() | {fail, term()} | {save_config, ct_suite:ct_config()}.
end_per_testcase(_Name, _Config) ->
    ok.

%% =============================================================================
%% Test Cases
%% =============================================================================

-spec test_app(ct_suite:ct_config()) -> ok | no_return().
test_app(_Config) ->
    ok = file:set_cwd("../../../../test/test_app"),
    State = init_test_app(),
    {Res, _} = spellcheck(State),
    ?assertEqual(ok, Res).

-spec no_good_files(ct_suite:ct_config()) -> ok | no_return().
no_good_files(_Config) ->
    ok = file:set_cwd("../../../../test/test_app"),
    {ok, State1} = init(),
    Files = {files, ["test_broken.erl", "non.existent.file"]},
    State2 = rebar_state:set(State1, spellcheck, [Files]),
    %% Our parsers don't crash on unparseable or non-existent files
    {Res, _} = spellcheck(State2),
    ?assertEqual(ok, Res).

-spec emits_warnings(ct_suite:ct_config()) -> ok | no_return().
emits_warnings(_Config) ->
    ok = file:set_cwd("../../../../test/test_app"),
    {ok, State1} = init(),
    Files = {files, ["src/test_warning.erl"]},
    State2 = rebar_state:set(State1, spellcheck, [Files]),
    %% Our parsers don't crash on unparseable or non-existent files
    Error = spellcheck(State2),
    ErrorMsg = get_error_msg(Error),
    %% Check warning message
    ?assertEqual(ErrorMsg, string:find(ErrorMsg, "src/test_warning.erl:3:")).

-spec ignore_regex(ct_suite:ct_config()) -> ok | no_return().
ignore_regex(_Config) ->
    ok = file:set_cwd("../../../../test/test_app"),
    {ok, State1} = init(),
    Files = {files, ["src/test_ignore_regex.erl"]},
    IgnoreRegEx = {ignore_regex, "[_@./#&+-=*]"},
    State2 = rebar_state:set(State1, spellcheck, [Files, IgnoreRegEx]),
    {Res, _} = spellcheck(State2),
    ?assertEqual(ok, Res).

-spec unicode(ct_suite:ct_config()) -> ok | no_return().
unicode(_Config) ->
    ok = file:set_cwd("../../../../test/test_app"),
    {ok, State1} = init(),
    Files = {files, ["src/test_unicode.erl"]},
    State2 = rebar_state:set(State1, spellcheck, [Files]),
    Error = spellcheck(State2),
    ErrorMsg = get_error_msg(Error),
    %% Check warning message
    ?assertEqual(ErrorMsg, string:find(ErrorMsg, "src/test_unicode.erl:8:")).

-spec custom_dictionaries_error(ct_suite:ct_config()) -> ok | no_return().
custom_dictionaries_error(_Config) ->
    ok = file:set_cwd("../../../../test/test_app"),
    {ok, State1} = init(),
    Files = {files, ["src/test_custom_dictionary.erl"]},
    Opts = [Files],
    State2 = rebar_state:set(State1, spellcheck, Opts),
    Error = spellcheck(State2),
    ErrorMsg = get_error_msg(Error),
    %% Check warning message
    ?assertEqual(ErrorMsg, string:find(ErrorMsg, "src/test_custom_dictionary.erl:8:")).

-spec custom_dictionaries(ct_suite:ct_config()) -> ok | no_return().
custom_dictionaries(_Config) ->
    ok = application:stop(sheldon),
    ok = file:set_cwd("../../../../test/test_app"),
    {ok, State1} = init(),
    Files = {files, ["src/test_custom_dictionary.erl"]},
    DefDict = {default_dictionary, "priv/dictionaries/default_dictionary.txt"},
    Dicts =
        {additional_dictionaries,
         ["priv/dictionaries/additional_dictionary_1.txt",
          "priv/dictionaries/additional_dictionary_2.txt"]},
    Opts = [Files, DefDict, Dicts],
    State2 = rebar_state:set(State1, spellcheck, Opts),
    {Res, _} = spellcheck(State2),
    ?assertEqual(ok, Res).

%% =============================================================================
%% Helpers
%% =============================================================================

-spec spellcheck(any()) -> any().
spellcheck(State) ->
    rebar3_sheldon_prv:do(State).

-spec init() -> any().
init() ->
    rebar3_sheldon:init(
        rebar_state:new()).

-spec init_test_app() -> any().
init_test_app() ->
    {ok, State1} = init(),
    Files =
        {files,
         ["*.config",
          "src/*.app.src",
          "src/*.sh",
          "src/*.erl",
          "src/*/*.erl",
          "include/*.hrl",
          "src/*.hrl"]},
    IgnoredFiles =
        {ignore,
         ["src/*_ignore.erl",
          "src/*_broken.erl",
          "src/*_warning.erl",
          "src/*_ignore_regex.erl",
          "src/*_unicode.erl",
          "src/*_dictionary.erl"]},
    IgnoreRegEx = {ignore_regex, "[_@./#&+-=*]"},
    rebar_state:set(State1, spellcheck, [Files, IgnoredFiles, IgnoreRegEx]).

-spec get_error_msg(tuple()) -> any().
get_error_msg({error, [_, [_, Error, _]]}) ->
    Error.
