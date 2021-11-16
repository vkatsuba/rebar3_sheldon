# rebar3_sheldon
[![Hex.pm Version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Build Status][gh badge]][gh]
[![Erlang Versions][erlang version badge]][gh]


A rebar plugin for spellchecking.

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_sheldon, "~> 0.1.3"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 spellcheck
===> Fetching rebar3_sheldon
===> Compiling rebar3_sheldon
===> Youre welcome. And if he has twins, we can do all kinds of neat experiments on them.:
test/test_SUITE.erl:1: The word "Speling" in string is unknown. Maybe you wanted to use "speeling" or "speiling" or ....?
test/test_SUITE.erl:2: The word "Commt" in string is unknown. Maybe you wanted to use "commit" or "commot" or "comdt" ...?
test/test_SUITE.erl:2: The word "fdfdf" in string is unknown.
test/test_SUITE.erl:3: The word "Unicode" in comment is unknown. Maybe you wanted to use "uncoded"?
```

## Config
Example:
```erlang
{spellcheck, [
    {files, ["src/*.erl", "src/*/*.erl", "include/*.hrl"]},
    {ignore, ["src/*_ignore.erl"]},
    {ignore_regex, "[_@./#&+-=*]"},
    {default_dictionary, "path/to/default_dictionary.txt"},
    {additional_dictionaries, ["path/to/custom_dictionary_1.txt", "path/to/custom_dictionary_2.txt"]},
    {options, #{dummy => option}}
]}.
```

<!-- Badges -->
[hexpm]: https://hex.pm/packages/rebar3_sheldon
[hexpm version]: https://img.shields.io/hexpm/v/rebar3_sheldon.svg?style=flat-square
[hexpm downloads]: https://img.shields.io/hexpm/dt/rebar3_sheldon.svg?style=flat-square
[gh]: https://github.com/vkatsuba/rebar3_sheldon/actions/workflows/ci.yml
[gh badge]: https://img.shields.io/github/workflow/status/vkatsuba/rebar3_sheldon/CI?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-23.0%20to%2024.1-blue.svg?style=flat-square
